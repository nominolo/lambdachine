{-# LANGUAGE BangPatterns #-}
module Lambdachine.Grin.Eval where

import Lambdachine.Utils
import Lambdachine.Id
import Lambdachine.Builtin
import Lambdachine.Grin

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Supply
import Control.Applicative

import Data.Generics.PlateDirect

newtype Loc = Loc Int
  deriving (Eq, Ord, Show)

instance Pretty Loc where
  ppr (Loc n) = char '#' <> int n

-- | A runtime value.
data RValue
  = RNode RValue [RValue]
  | RInt Integer
  | RHole
  | RLoc Loc
  | RGlobal Var
  deriving (Eq, Ord, Show)

instance Uniplate RValue where
  uniplate (RNode v vs) = plate RNode |* v ||* vs
  uniplate r = plate r

instance Pretty RValue where
  ppr (RInt n) = text (show n)
  ppr (RLoc l) = ppr l
  ppr RHole = text "[]"
  ppr (RNode x xs) = parens (hsep (map ppr (x:xs)))
  ppr (RGlobal v) = ppr v

data Heap = Heap (M.Map Loc RValue) (Supply Loc)

newHeap :: IO Heap
newHeap = do
  Heap M.empty <$> newSupply (Loc 1) (\(Loc a) -> Loc (a + 1))

fetch :: Heap -> Loc -> RValue
fetch (Heap m _) l = expectJust "fetch" $ M.lookup l m

newtype LocalEnv = LocalEnv (M.Map Id RValue)
data GlobalEnv = GlobalEnv
  { gblFuns :: M.Map Id FunDef }

moduleGblEnv :: Module -> GlobalEnv
moduleGblEnv mdl =
  GlobalEnv
  { gblFuns = M.fromList [ (funDefName f, f) | f <- moduleFuns mdl ] }

lookupGblEnv :: GlobalEnv -> Id -> Maybe FunDef
lookupGblEnv GlobalEnv{ gblFuns = funs } f = M.lookup f funs

emptyEnv :: LocalEnv
emptyEnv = LocalEnv M.empty
lookupEnv :: LocalEnv -> Id -> Maybe RValue
lookupEnv (LocalEnv lcl) var = M.lookup var lcl

extendEnv :: LocalEnv -> Value -> RValue -> LocalEnv
extendEnv env (Var v) x = extend_env env v x
extendEnv env_ (Node v vs) (RNode x xs) = go env_ (v:vs) (x:xs)
 where
   go !env [] [] = env
   go !env (a:as) (b:bs) = go (extend_env env a b) as bs

extend_env (LocalEnv m) var val = LocalEnv (M.insert var val m)

extendEnvN :: LocalEnv -> [Value] -> [RValue] -> LocalEnv
extendEnvN env_ vs_ rs_ = go env_ vs_ rs_
 where 
   go env [] [] = env
   go env (v:vs) (r:rs) = go (extendEnv env v r) vs rs
   go _ _ _ = error $ "extendEnvN:" ++ pretty (vs_, rs_)

val :: LocalEnv -> Value -> RValue
val env val_ = case val_ of
  Lit n -> RInt n
  Var v | isTopLevelId v -> RGlobal v
        | isDataConId v -> RGlobal v
        | otherwise -> lu v
  Void -> RHole
  Hole n -> RHole
  Node v vs -> RNode (val env (Var v)) (map (val env . Var) vs)
 where
   lu v = 
     expectJust ("val:" ++ show (v, idDetails v) ) $ lookupEnv env v

-- | Return all the 'Loc's (pointers) contained within this 'RValue'.
rval_locs :: RValue -> [Loc]
rval_locs r = [ l | RLoc l <- universe r ]

pprHeapLocs :: Heap -> Bool -> [Loc] -> [PDoc]
pprHeapLocs (Heap m _) recurse locs = go S.empty locs
 where
   go _ [] = []
   go visited (l:ls)
     | l `S.member` visited = go visited ls
     | otherwise =
       case M.lookup l m of
         Nothing -> go (S.insert l visited) ls
         Just val ->
           let ls' | recurse = rval_locs val ++ ls
                   | otherwise = ls
           in (ppr l <> text "=>" <> ppr val) : 
              go (S.insert l visited) ls'

eval :: GlobalEnv -> Heap -> LocalEnv -> Int -> Expr -> IO (RValue, Heap)
eval gbl hp@(Heap m ls) env n expr = case expr of
  e1 :>>= (p :-> e2) -> do
    (v, hp') <- eval gbl hp env n e1
    pprint $ indent (n+2) $ text "--" <+> ppr p <> char '=' <> ppr v
           <+> brackets (hsep $ punctuate (char ';') $ 
                         pprHeapLocs hp' False (rval_locs v))
    let env' = extendEnv env p v
    eval gbl hp' env' n e2
  e1 :>> e2 -> do
    (_, hp') <- eval gbl hp env n e1
    eval gbl hp' env n e2
  Unit x -> do
    pprint $ indent n $ ppr expr
    return (val env x, hp)
  Store v -> do
    pprint $ indent n $ ppr expr
    case split2 ls of
      (a, ls') ->
        let 
          l = supplyValue a
          hp' = Heap (M.insert l (val env v) m) ls'
        in return (RLoc l, hp')
  Case e alts -> do
    pprint $ indent n $ keyword "case" <+> ppr e
    case val env (Var e) of
      RNode (RGlobal t) vs ->
        let (xs, k) = selalt t alts
            env' = extendEnvN env xs vs
        in do
          pprint $ indent (2+n) $ text "--" <+> ppr_bind_pairs xs vs
          eval gbl hp env' n k
      RInt i ->
        let k = selalt_lit i alts
        in eval gbl hp env n k
      RGlobal t ->
        let ([], k) = selalt t alts
        in do
          eval gbl hp env n k
      v ->
        error $ "case:" ++ pretty v
        
  App f xs
    | isTopLevelId f -> do
        call_toplevel_fun n f (map (val env . Var) xs)
  Prim op xs
    | show (idName op) == "GHC.Prim.+#" -> do
      pprint $ indent n $ ppr expr
      case map (val env . Var) xs of
        [RInt n, RInt m] -> return (RInt (m+n), hp)
    | show (idName op) == "GHC.Prim.>#" -> do
      pprint $ indent n $ ppr expr
      case map (val env . Var) xs of
        [RInt n, RInt m] -> 
          return (RNode (RGlobal (if n > m then trueDataConId else falseDataConId)) [], hp)
  Eval upd x -> do
    let v = val env (Var x)
    pprint $ indent n $
      char '(' <+> text "-- eval p/" <> ppr x <> char '=' <> ppr v
    case v of
      RLoc l -> do
        let obj = fetch hp l
        pprint $ indent (n+2) $
          keyword "fetch" <+> text "p" <+> text "-- => " <> ppr obj
        case obj of
          obj@(RNode (RGlobal tag) vals)
            | isDataConId tag -> do
                pprint $ indent n $ char ')'
                return (obj, hp)
            | isTopLevelId tag -> do
                (obj', hp') <- call_toplevel_fun (n+2) tag vals
                return (obj', hp')
      obj@(RNode (RGlobal tag) vals)
        | isDataConId tag -> do
            pprint $ indent n $ char ')'
            return (obj, hp)
  _ -> error (pretty expr)
 where
--   selalt tag ((
   selalt tag ((Node t vs :> e):alts) 
     | tag == t = (map Var vs, e)
     | otherwise = selalt tag alts
--   selalt tag ((Var 
   selalt_lit n ((p :> e):alts) = e
   call_toplevel_fun n f vals = do
     let Just fun = lookupGblEnv gbl f
         args = funDefArgs fun
         body = funDefBody fun
     pprint $ indent n $
       ppr f <> linebreak <>
       char '(' <+> text "--" <+> 
            align (ppr f <+> ppr_bind_pairs args vals <> linebreak <>
                   (sep (pprHeapLocs hp True (concatMap rval_locs vals))))
     let env' = extendEnvN env (map Var args) vals
     r <- eval gbl hp env' (n+2) body
     pprint $ indent n $ char ')'
     return r
   ppr_bind_pairs args vals =
     hsep (map (\(a, v) -> ppr a <> char '=' <> ppr v) (zip args vals))
     

runTest1 :: Module -> IO ()
runTest1 mdl = do
  let ((_,e):_) = moduleCafs mdl
  let gbl = moduleGblEnv mdl
  let env0 = emptyEnv
  hp0 <- newHeap
  _ <- eval gbl hp0 env0 0 e
  return ()
        
--  App f args ->
    
--eval :: 

