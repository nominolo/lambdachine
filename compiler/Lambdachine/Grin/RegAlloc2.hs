{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
module Lambdachine.Grin.RegAlloc where

import Lambdachine.Grin.Bytecode

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State

-- -------------------------------------------------------------------
-- Liveness analysis and bytecode simplification

newtype RnEnv = RnEnv (

newtype RnM a = RnM (State RnState a)
  deriving (Functor, Applicative, Monad)

data RnState = RnState
  { instrIdx :: !Int
  , nextName :: !Int
  , nameMap :: M.Map BcVar RegName }

mkRnState :: RnState
mkRnState = RnState { instrIdx = 0
                    , nextName = 0
                    , nameMap = M.empty }

rnRnM :: RnM a -> a
rnRnM (RnM s) = runState

getInstrId :: RnM Int
getInstrId = RnM $ do
  s <- get
  let !n = instrIdx s
  put $! s{ instrIdx = n + 1 }
  return n

getNextName :: RnM RegName
getNextName = RnM $ do
  s <- get
  let !n = nextName s
  put $! s{ nextName = n + 1 }
  return (RegName n)

newtype RegName = RegName Int
  deriving (Eq, Ord)

renameBCO :: BytecodeObject BcVar BcConst
          -> BytecodeObject Int Int
renameBCO BCO{ bcoCode = code } =
  let c = renameCode code in undefined

--renameCode :: [BcInstr BcVar BcConst] -> 
renameCode code = runRnM $ rename_code code

rename_code (Move d s) = do
  s' <- useVar s
  d' <- defVar d

-- -------------------------------------------------------------------

type RAInstr = BcInstr Int BcConst

zero :: S.Set a
zero = S.empty

one :: a -> S.Set a
one = S.singleton

two :: Ord a => a -> a -> S.Set a
two x y = S.insert x (S.singleton y)

defUse :: Ord r => BcInstr r c -> (Maybe r, S.Set r)
defUse (Move dst src) = (Just dst, one src)
defUse (LoadK dst _)  = (Just dst, zero)
defUse (LoadG dst _)  = (Just dst, zero)
defUse (LoadC dst _)  = (Just dst, zero)
defUse (LoadF dst _)  = (Just dst, zero)
defUse (LoadBlackhole dst) = (Just dst, zero)
defUse (BinR _ _ dst src1 src2) = (Just dst, two src1 src2)
defUse (BinC _ _ dst src _) = (Just dst, one src)
defUse (Eval dst src) = (Just dst, one src)
defUse (Case src alts) = (Nothing, one src)
defUse (Ret1 src) = (Nothing, one src)
defUse (Fetch dst src _) = (Just dst, one src)
defUse (Update base _ src) = (Nothing, two base src)
defUse (Store dst src srcs) = (Just dst, S.insert src (S.fromList srcs))
defUse (MkAp dst src srcs) = (Just dst, S.insert src (S.fromList srcs))
defUse (Call rslt src srcs) = (rslt, S.insert src (S.fromList srcs))
defUse Nop = (Nothing, zero)

