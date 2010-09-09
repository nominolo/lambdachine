{-# LANGUAGE GADTs, MultiParamTypeClasses, PatternGuards #-}
module Lambdachine.Grin.RegAlloc where

import Lambdachine.Grin.Bytecode
import Lambdachine.Grin.Analyse
import Lambdachine.Utils

import Compiler.Hoopl
import Data.Vector ( Vector )
import qualified Data.Vector as Vec
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Generics.Uniplate.Direct

allocRegs :: BytecodeObject -> BytecodeObject' FinalCode
allocRegs bco@BcoCon{} = -- this is just silly
  BcoCon{ bcoType = bcoType bco
        , bcoDataCon = bcoDataCon bco
        , bcoFields = bcoFields bco }
allocRegs bco0@BcObject{ bcoCode = code } =
  BcObject{ bcoType = bcoType bco
          , bcoCode = finaliseCode (bcoArity bco0) code'
          , bcoGlobalRefs = bcoGlobalRefs bco
          , bcoConstants = bcoConstants bco
          , bcoFreeVars = bcoFreeVars bco }
 where
   (bco, live_facts) =
     runM $ analyseAndRewriteBCOBwd bco0
              livenessAnalysis2 noFacts
   code' = allocRegsGraph live_facts (bcoCode bco)

allocRegsGraph :: FactBase LiveVars -> Graph BcIns O C -> LinearCode
allocRegsGraph lives g = mkAllocMap (lineariseCode lives g)
--   GMany NothingO blocks' NothingO
--  where blocks' = allocRegsBody blocks

--allocRegsBody :: Body BcIns -> [LinearIns]
--allocRegsBody body = 

-- | Finalise step in generating executable bytecode.  Does the
-- following:
--
--   * Remove some redundant instructions (labels and gotos to adjacent
--     instructions).
--
--   * Turn jump labels into absolute addresses.
--
--   * Calculate the frame size (number of registers needed).
--
finaliseCode :: Int -> LinearCode -> FinalCode
finaliseCode arity (LinearCode code0 lives labels) =
  FinalCode framesize code
 where
   framesize = arity `max` Vec.maximum (Vec.map S.size lives)
   code1 = Vec.imap (\offs ins -> (ins, keep offs ins)) code0
   code = Vec.imap (adjust_idx new_labels0) 
        . Vec.map fst
        . Vec.filter snd
        $ code1
   code0_len = Vec.length code0

   keep :: Int -> LinearIns -> Bool
   keep _ (Fst _) = False
   keep _ (Mid _) = True
   keep i (Lst lins) = case lins of
       Goto l
         | i + 1 < code0_len, Fst (Label l') <- code0 Vec.! (i + 1)
         -> l /= l'  -- only keep if jump is not to next instruction
       _ -> True

   new_labels0 = fst $ Vec.foldl' calc_offs (M.empty, 0) code1
    where
      calc_offs :: (M.Map Label Int, Int) -> (LinearIns, Bool)
                -> (M.Map Label Int, Int) 
      calc_offs (mp, new_idx) (Fst (Label l), keep) =
        (M.insert l new_idx mp, new_idx)
      calc_offs (mp, new_idx) (i, keep) =
        (mp, if keep then new_idx + 1 else new_idx)

   adjust_idx new_labels i (Mid ins) =
     Mid $ mapLabels (\l -> (new_labels M.! l)) ins
   adjust_idx new_labels i (Lst ins) =
     Lst $ mapLabels (\l -> (new_labels M.! l)) ins

data LinearCode = LinearCode
  { lc_code    :: Vector LinearIns
  , lc_liveIns :: Vector LiveVars
  , lc_labels  :: M.Map Label Int
  }

instance Pretty LinearCode where
  ppr (LinearCode is ls lbls) =
    --ppr lbls $+$
    vcat (Vec.toList (Vec.zipWith pp is ls))
   where pp i l = fillBreak 30 (ppr i) <+> ppr l

lineariseCode :: FactBase LiveVars -> Graph BcIns O C -> LinearCode
lineariseCode live_facts g@(GMany (JustO entry) body NothingO) =
   LinearCode lin_code (liveIns live_facts lin_code) labels
 where
   lin_code = Vec.fromList $ concat $ 
                lineariseBlock entry : map lineariseBlock body_blocks
   body_blocks = postorder_dfs g  -- excludes entry sequence
   labels = Vec.ifoldl' ins_if_label M.empty lin_code
   ins_if_label :: M.Map Label Int -> Int -> LinearIns -> M.Map Label Int
   ins_if_label m n (Fst (Label l)) = M.insert l (n+1) m
   ins_if_label m _ _ = m
--   lin_block = 

lineariseBlock :: Block BcIns e x -> [LinearIns]
lineariseBlock blk = entry_ins (map Mid middles ++ tail_ins)
 where
   (entry, middles, tail) = blockToNodeList blk
   entry_ins :: [LinearIns] -> [LinearIns]
   entry_ins = case entry of
                 JustC n -> (Fst n :) 
                 NothingC -> id
   tail_ins :: [LinearIns]
   tail_ins = case tail of
                JustC x -> [Lst x]
                NothingC -> []

-- | Calculate the live-in variables at each instruction.
liveIns :: FactBase LiveVars -> Vector LinearIns -> Vector LiveVars
liveIns global_live_outs inss =
  Vec.postscanr' calcLives S.empty inss
 where
   calcLives (Lst ins) live_out = live ins global_live_outs
   calcLives (Mid ins) live_out = live ins live_out
   calcLives (Fst ins) live_out = live ins live_out

allRegs :: S.Set BcVar
allRegs = S.fromList $ map BcReg [0..255]

mkAllocMap :: LinearCode -> LinearCode -- M.Map BcVar BcVar
mkAllocMap lc@(LinearCode code0 lives lbls) =
  let (alloc, _, _) = Vec.foldl' alloc1 (M.empty, allRegs, S.empty) lives
      code' = assignRegs alloc code0
  in
    if not (verifyAlloc alloc lives) then
      let lc' = LinearCode code' (Vec.map (S.map (alloc M.!)) lives) lbls in
      error ("BUG-IN-REGALLOC\n" ++ pretty lc ++ "\n\n" ++ pretty alloc ++ "\n" ++ pretty lc')
     else
       LinearCode code' lives lbls
 where
   verifyAlloc alloc lives =
     Vec.and (Vec.map (\l -> S.size (S.map (alloc M.!) l) == S.size l) lives)
   assignRegs alloc code =
     Vec.map (transformBi (alloc M.!)) code
   alloc1 (alloc, avail0, prev_live) live =
     let freed_vars = prev_live `S.difference` live
         avail1 = (S.map (alloc M.!) freed_vars) `S.union` avail0
         alloc'd_vars = live `S.difference` prev_live
         (alloc', avail') = S.fold alloc2 (alloc, avail1) alloc'd_vars
     in (alloc', avail', live)
   alloc2 x st@(alloc, avail)
     | Just r <- M.lookup x alloc = (alloc, S.delete r avail)
     | otherwise =
       case x of
         BcReg r -> (M.insert x x alloc, S.delete x avail)
         _ ->
           let (reg, avail') = S.deleteFindMin avail in
           (M.insert x reg alloc, avail')



{-
deleteFindMinN :: Ord a => Int -> S.Set a -> ([a], S.Set a)
deleteFindMinN n s
  | n <= 0 = ([], s)
  | otherwise =
    let (m, s') = S.deleteFindMin s
        (ms, s'') = deleteFindMinN (n - 1) s'
    in (m:ms, s'')
-}
