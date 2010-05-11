{-# LANGUAGE ViewPatterns, PatternGuards, BangPatterns #-}
-- | Reference interpreter for Core
module Lambdachine.Eval where

--import Lambdachine.Core
import Lambdachine.Utils
import Lambdachine.Name
import Lambdachine.Stg
--import Lambdachine.Utils.Pretty hiding ( (<$>) )

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Map ( (!) )
import Control.Applicative

import Data.Supply

newtype Addr = Addr Int
  deriving (Eq, Ord, Show)

data RegVal
  = UInt Literal
  | Ptr Addr
  deriving (Eq, Ord)

data HeapObj
  = Constr DataCon [RegVal]
  | Func [Var] Term LocalEnv
    -- ^ A function or a thunk.  @THUNK e env@ = @Func [] e env@.
    -- The local env must bind all free variables of @e@.
  | Pap Addr [RegVal]
    -- ^ A partial application.  The 'Addr' argument points to an
    -- evaluated 'Func'.
  | Blackhole

isValue :: HeapObj -> Bool
isValue (Constr _ _) = True
isValue (Func (_:_) _ _) = True
isValue _ = False

data LocalEnv = LocalEnv !(M.Map Var RegVal) !(Supply Unique)
data Heap = Heap (M.Map Addr HeapObj) (Supply Addr)

data EvalContext
  = Upd Addr LocalEnv
  | Alt Var [Alt] LocalEnv
  | AppC [RegVal]

type Stack = [EvalContext]

eval :: LocalEnv -> Heap -> Stack -> Term 
     -> Maybe (LocalEnv, Heap, Stack, Term)
eval lcl hp st term_ = case viewTerm term_ of
  -- These cases don't cover a free variable or a (reference to a)
  -- value in an empty context.  We simple stop in that case.
  term@(Atom (lookupAtomMaybe lcl -> a))
    | Just n@(UInt lit) <- a, (Alt v alts lcl':st') <- st
    -> match_alt_lit lit alts (lcl' // [(v, n)]) hp st'
    | Just (Ptr addr) <- a, s:st' <- st
    -> case fetch hp addr of
         Func [] e lcl' ->
           let hp' = store hp addr Blackhole in
           Just (lcl', hp', Upd addr lcl : st, e)
         Blackhole -> Nothing
         -- From now on `obj` must be a value.
         obj | AppC args <- s, Func (_:_) _ _ <- obj
             -> apply addr args lcl hp st'
         obj | AppC args <- s, Pap _ _ <- obj
             -> apply addr args lcl hp st'
         obj | Upd addr' lcl' <- s ->
           let hp' = store hp addr' obj in
           Just (lcl, hp', st', term)
         obj | Alt v alts lcl' <- s ->
           match_alt obj alts (lcl' // [(v, Ptr addr)]) hp st'
  App fv@(lookupVar lcl -> Ptr f) args
    | Func params@(_:_) e lcl' <- fetch hp f
    -> apply f (map (lookupAtom lcl) args) lcl hp st
    | Func [] _ _ <- fetch hp f
    -> -- function is a thunk.  dispatches to thunk case above
       Just (lcl, hp, AppC (map (lookupAtom lcl) args) : st, var fv)
    | Pap f' vals <- fetch hp f
    -> apply f' (vals ++ map (lookupAtom lcl) args) lcl hp st

  LetRec rs term ->
    alloc hp (map fst rs) lcl $ \hp' lcl' ->
      let
        hp'' = storeTerms hp' lcl' rs 
      in
        Just (lcl', hp'', st, term)
  Case e v alts ->
    Just (lcl, hp, Alt v alts lcl : st, e)
  POp op args ->
    Just (lcl, hp, st, prim_op op (map (lookupAtom lcl) args))
  _ -> Nothing

match_alt :: HeapObj -> [Alt] -> LocalEnv -> Heap -> Stack 
          -> Maybe (LocalEnv, Heap, Stack, Term)
match_alt (Constr dcon vals) alts lcl hp st = go alts
 where
   go [] = Nothing
   go [(DEFAULT, e)] = Just (lcl, hp, st, e)
   go ((DataAlt dcon' vars, e):alts') 
     | dcon == dcon' =
       Just (lcl // zip vars vals, hp, st, e)
     | otherwise = go alts'
match_alt _ [(DEFAULT, e)] lcl hp st =
  Just (lcl, hp, st, e)

match_alt_lit :: Literal -> [Alt] -> LocalEnv -> Heap -> Stack
              -> Maybe (LocalEnv, Heap, Stack, Term)
match_alt_lit lit alts lcl hp st = go alts
 where
   go [] = Nothing
   go [(DEFAULT, e)] = Just (lcl, hp, st, e)
   go ((LitAlt l', e):alts')
    | l' == lit = Just (lcl, hp, st, e)
    | otherwise = go alts'
   go as = error $ "match_alt_lit: " ++ pretty (lit, alts)
                   
prim_op :: PrimOp -> [RegVal] -> Term
prim_op op [UInt x, UInt y] =
  Atom $ Lit $
    (case op of Add -> (+); Sub -> (-); Mul -> (*); Div -> div) x y

-- INVARIANT: addr -> PAP or FUNC (not THUNK)
apply :: Addr -> [RegVal] -> LocalEnv -> Heap -> Stack
      -> Maybe (LocalEnv, Heap, Stack, Term)
apply faddr args lcl hp st
  | Func params@(_:_) e lcl' <- fetch hp faddr
  = let
      n = length params
      m = length args
      result
        | m == n =
          Just (substArgVals params args lcl', hp, st, e)
        | m > n =
          Just (substArgVals params (take n args) lcl', hp,
                AppC (drop n args) : st, e)
        | m < n =
          let (hp', v, a) = allocFresh hp (Pap faddr args)
          in Just (lcl // [(v, Ptr a)], hp', st, var v)
    in result
  | Pap faddr' args1 <- fetch hp faddr
  = apply faddr' (args1 ++ args) lcl hp st

(//) :: LocalEnv -> [(Var, RegVal)] -> LocalEnv  
(LocalEnv lcl s) // ms = LocalEnv (M.fromList ms `M.union` lcl) s

newHeap :: IO Heap
newHeap = do
  Heap M.empty <$> newSupply (Addr 1) (\(Addr a) -> Addr (a + 1))

traceEval :: Term -> IO ()
traceEval term = do
  hp <- newHeap
  lcl <- mkEmptyEnv
  go lcl hp [] term
 where
   go lcl hp st (Pos p t) =
     print p >> go lcl hp st t
   go lcl hp st t =
     --pprint (lcl, st, t) >>
     case eval lcl hp st t of
       Nothing -> putStr "Done: " >> pprint (t, st, lcl, hp)
       Just (lcl', hp', st', t') ->
         --pprint t' >>
         go lcl' hp' st' t'
  
    
fetch :: Heap -> Addr -> HeapObj
fetch (Heap m _) a = m M.! a

store :: Heap -> Addr -> HeapObj -> Heap
store (Heap m s) a v = Heap (M.insert a v m) s

substArgs :: LocalEnv -> [Var] -> [Atom] -> LocalEnv -> LocalEnv
substArgs lcl [] [] !lcl' = lcl'
substArgs lcl (v:vs) (Lit l : as) !lcl' = 
  substArgs lcl vs as (extendEnv lcl' v (UInt l))
substArgs lcl (v:vs) (Var v' : as) !lcl' =
  substArgs lcl vs as $
    extendEnv lcl' v (expectJust msg (lookupEnv lcl v'))
 where
   msg = "substArgs: " ++ pretty (v', lcl)

substArgVals :: [Var] -> [RegVal] -> LocalEnv -> LocalEnv
substArgVals [] [] !lcl' = lcl'
substArgVals (v:vs) (r:rs) !lcl' =
  substArgVals vs rs (extendEnv lcl' v r)

-- | Allocate vars on the heap (fill them with black holes).
alloc :: Heap -> [Var] -> LocalEnv -> (Heap -> LocalEnv -> a) -> a
alloc (Heap m s) vars lcl_ kont = go vars m s lcl_
 where
   go [] m s lcl = kont (Heap m s) lcl
   go (v:vs) m s lcl =
     case split2 s of
       (sa, s') | a <- supplyValue sa ->
         go vs (M.insert a Blackhole m) s' (extendEnv lcl v (Ptr a))

allocFresh :: Heap -> HeapObj -> (Heap, Var, Addr)
allocFresh (Heap m s) obj =
  case split2 s of
    (supplyValue -> a, s') ->
      let v = name ('v':pretty a) in
      (Heap (M.insert a obj m) s', v, a)

extendEnv :: LocalEnv -> Var -> RegVal -> LocalEnv
extendEnv (LocalEnv m s) var val = LocalEnv (M.insert var val m) s

lookupAtom :: LocalEnv -> Atom -> RegVal
lookupAtom lcl = expectJust "lookupAtom" . lookupAtomMaybe lcl

mkEmptyEnv :: IO LocalEnv
mkEmptyEnv = LocalEnv M.empty <$> newUniqueSupply

lookupAtomMaybe :: LocalEnv -> Atom -> Maybe RegVal
lookupAtomMaybe lcl (Lit l) = Just (UInt l)
lookupAtomMaybe lcl (Var v) = lookupEnv lcl v

lookupVar :: LocalEnv -> Var -> RegVal
lookupVar lcl var = expectJust "lookupVar" $ lookupEnv lcl var

lookupEnv :: LocalEnv -> Var -> Maybe RegVal
lookupEnv (LocalEnv lcl _) var = M.lookup var lcl

--freshLocalVars :: LocalEnv -> [RegVal] -> (

storeTerms :: Heap -> LocalEnv -> [(Var, Obj)] -> Heap
storeTerms h lcl mappings = go mappings h
 where
   go [] h = h
   go ((v, m):ms) h =
     let Just (Ptr a) = lookupEnv lcl v
         !h' = store h a hp_val
         hp_val =
           case m of
             Con dcon args ->
               Constr dcon (map (lookupAtom lcl) args)
             Fun _fvs args body | (_:_) <- args ->
               Func args body lcl
             Thunk _fvs body ->
               Func [] body lcl
     in go ms h'

 
instance Pretty Addr where
  ppr (Addr i) = char '#' <> int i
  
instance Pretty RegVal where
  ppr (UInt l) = ppr l
  ppr (Ptr p) = ppr p

instance Pretty Heap where
  ppr (Heap m _) = char '<' <> text "heap" <+> ppr m <> char '>'
instance Pretty LocalEnv where
  ppr (LocalEnv m _) = char '<' <> text "env" <+> ppr m <> char '>'
  
instance Pretty HeapObj where
  ppr (Constr dcon vals) =
    text "CON" <> parens (text dcon <+> hsep (map ppr vals))
  ppr (Func [] term _) = text "THUNK" <> parens (ppr term)
  ppr (Func ps term _) =
    text "FUN" <> parens (hsep (map ppr ps) <+> text "-> ...")
  ppr (Pap f vals) =
    text "PAP" <> parens (ppr f <+> hsep (map ppr vals))
  ppr Blackhole = text "BLACKHOLE"

instance Pretty EvalContext where
  ppr (Upd addr env) =
    text "Upd" <+> ppr addr <+> char '*' <+> ppr env
  ppr (Alt var alts env) =
    text "case * of" <+> hsep (map (ppr . fst) alts) <+> ppr env
  ppr (AppC vals) =
    text "*" <+> hsep (map ppr vals)
    

tst1 = LetRec [(a, Con "I#" [Lit 42])] (var a)
  where
    [a,b,c,d] = map (name . (:[])) "abcd"

-- | Add labels wherever computation branches could occur.
addLabels :: Supply Int -> Term -> Term
addLabels = add_labels True
 where
   add_labels br s term = case term of 
     Atom (Var _) -> Pos (supplyValue s) term
     App v as -> Pos (supplyValue s) term
     POp o as -> term
     Case t v [(altcon, e)] ->
       case split2 s of
         (s1, s2) ->
           Case (addLabels s1 t) v [(altcon, addLabels s2 e)]
     Case t v alts ->
       case split2 s of
         (s1, s2) ->
           Case (add_labels True s1 t) v (add_labels_alts s2 alts)
     LetRec binds body ->
       case split2 s of
         (s1, s2) ->
           LetRec (add_labels_binds s1 binds) (add_labels True s2 body)

   add_labels_alts _ [] = []
   add_labels_alts s ((alt, term):alts) =
     case split2 s of
       (s1, s2) -> (alt, addLabels s1 term) : add_labels_alts s2 alts
  
   add_labels_binds _ [] = []
   add_labels_binds s ((v, Fun fvs ps t):binds) =
     case split2 s of
       (s1, s2) -> (v, Fun fvs ps (addLabels s1 t)) :
                   add_labels_binds s2 binds
   add_labels_binds s ((v, Thunk fvs t):binds) =
     case split2 s of
       (s1, s2) -> (v, Thunk fvs (addLabels s1 t)) :
                   add_labels_binds s2 binds
   add_labels_binds s (b:binds) = b : add_labels_binds s binds
    
tst2 = 
  LetRec [(plusInt, Fun [] [a,b] $
           Case (var a) u $
           [(DataAlt "I#" [a],
             Case (var b) u $
             [(DataAlt "I#" [x],
               Case (POp Add [Var a, Var x]) r $
               [(DEFAULT, LetRec [(s, Con "I#" [Var r])] $
                          (var s))])])])
--         (sum, Fun [] [l] $
--                      Case (var l) $
--                      [(DataAlt nilDCon [], var zero)
--                      ,(DataAlt consDCon [x,xs],
--                        Case (App sum [xs]) $
--                        [(DataAlt "I#" [s],var x)])])]
         ,(zero, Con "I#" [Lit 0])
         ,(one, Con "I#" [Lit 1])
         ,(two, Con "I#" [Lit 2])] $
       App plusInt [Var one, Var two]
 where
   [sum,l,zero,one,two,x,xs,s,plusInt,a,b,r,u] 
     = map name ["sum","l","zero","one","two","x","xs","s",
                 "plusInt","a","b","r","_"]
   nilDCon = "[]"
   consDCon = "(:)"

let1 :: Var -> Term -> Term -> Term
let1 v t body = Case t v [(DEFAULT, body)]

tst3 =
  LetRec [(plusInt, Fun [] [a,b] $
           Case (var a) u $
           [(DataAlt "I#" [a],
             Case (var b) u $
             [(DataAlt "I#" [x],
               let1 r (POp Add [Var a, Var x]) $
               LetRec [(s, Con "I#" [Var r])] $
               (var s))])])
         ,(dec, Fun [] [x] $
                Case (var x) u $
                [(DataAlt "I#" [x],
                  let1 a (POp Sub [Var x, Lit 1]) $
                  LetRec [(b, Con "I#" [Var a])] $
                  var b)])
         ,(sum, Fun [] [x,xs] $
                Case (var x) u $
                [(DataAlt "I#" [x'],
                  Case (var x') u $
                  [(LitAlt 0, var xs)
                  ,(DEFAULT,
                    let1 a (App dec [Var x]) $
                    let1 b (App plusInt [Var x, Var xs]) $
                    App sum [Var a, Var b])])])
         ,(zero, Con "I#" [Lit 0])
         ,(two, Con "I#" [Lit 2])
         ] $
  let1 r (App sum [Var two]) $ App r [Var zero] --, Var zero]

 where
   [sum,dec,l,zero,one,two,x,x',y,xs,s,plusInt,a,b,r,u] 
     = map name ["sum","dec","l","zero","one","two","x","x'","y","xs","s",
                 "plusInt","a","b","r","_"]

tst4 =
  LetRec
    [(f1, Fun [] [b] $
         Case (var b) u [(DataAlt "I#" [b'],
         let1 b'' (POp Add [Var b', Lit 1]) $
         LetRec [(r, Con "I#" [Var b''])] $
         var r)])
    ,(nilCon, Con "[]" [])
    ,(mymap, Fun [] [f, l] $
           Case (var l) u $
           [(DataAlt "[]" [], var nilCon)
           ,(DataAlt ":" [x,xs],
             LetRec
               [(y, Thunk [f, x] (App f [Var x]))
               ,(ys, Thunk [mymap, f, xs] (App mymap [Var f, Var xs]))
               ,(r, Con ":" [Var y, Var ys])] $
             var r)])
    ,(l, Thunk [] $ num_list "list" [1..5])] $
  App mymap [Var f1, Var l]
 where
   nilCon = name "nil"
   [f, f1, u, b, b', b'', r, mymap, l, x, xs, y, ys]
     = map name ["f", "f1", "_", "b", "b'", "b''", "r"
                ,"map", "l", "x", "xs", "y", "ys"] :: [Var]
   num_list :: String -> [Int] -> Term
   num_list n list = 
      LetRec (go 0 [] list) $ var (name (n ++ ":0"))
    where
      go _ acc [] = acc
      go c acc (l:ls) =
        let lit_name = name (n ++ "$" ++ show c)
            cons_name = name (n ++ ":" ++ show c)
            next_name
              | [] <- ls = nilCon
              | otherwise = name (n ++ ":" ++ show (c+1))
        in
        go (c+1) ((lit_name, Con "I#" [Lit l]) :
                  (cons_name, Con ":" [Var lit_name, Var next_name]) :
                  acc) ls
   

tr_eval t = do
  s <- newNumSupply
  let e = addLabels s t
  pprint e
  traceEval e
                
{-
eval :: Expr Id -> LocalEnv -> Stack -> Heap 
     -> Maybe (Expr Id, Stack, Heap)
eval (Lit l) lcl (CaseCont alts : s) h =
  Just (select_alt (LitVal l) alts, s, h)  -- RET+CASECON
eval (Var v) lcl (CaseCont alts : s) h =
  Nothing
eval _ _ _ = Nothing

select_lit_alt :: Val -> [Alt Id] -> Expr Id
select_lit_alt l alts =
  case alts of
    (DEFAULT, _, e) : alts' -> go alts' e
    _ -> go alts (error "Non-exhaustive pattern match")
 where
   go ((LitAlt l', [], e) : ps) dflt
     | l == l' = e
     | otherwise = go ps dflt
   go [] dflt = dflt
-}
{-
type Stack = [StackCont]

data StackCont
  = CaseCont [Alt Id]
  | UpdCont Int
  | AppCont [Val]

type Heap = IM.IntMap Val

type LocalEnv = M.Map Id Val


data Val
  = LitVal Literal
  | PtrVal Int
-}
