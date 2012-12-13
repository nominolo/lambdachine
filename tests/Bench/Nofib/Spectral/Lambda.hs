{-# LANGUAGE NoImplicitPrelude, CPP, MagicHash #-}

#ifdef BENCH_GHC
module Main( main ) where
import Prelude
#else
module Bench.Nofib.Spectral.Lambda where

import GHC.Base
import GHC.Num
import GHC.Show
import Data.List
import Data.Tuple
import Data.Maybe
#endif

-- Experiments with Higher-Order mappings over terms.
-- Speed comparisons of passing environments in a monad, versus explicitly.

--import System.Environment


#ifdef BENCH_GHC
main :: IO ()
--main = putStrLn (runSimple 2000) >> putStrLn (runMonad 2000)
main = print bench
#endif

test = testSimple && testMonad
bench = benchSimple && benchMonad

testSimple = runSimple 50 == "Con 1275"
testMonad = runMonad 50 == "1275  []"

benchSimple = runSimple 2000 == "Con 2001000"
benchMonad = runMonad 2000 == "2001000  []"


error' _ = let x = x in x

{-
main = do { mainSimple ; mainMonad }

mainSimple =
    do  args <- getArgs
	if null args
	   then putStrLn "Args: number-to-sum-up-to"
	   else putStrLn (show (simpleEval [] (App sum0 (Con (read(head args))))))

mainMonad =
    do  args <- getArgs
	if null args
	   then putStrLn "Args: number-to-sum-up-to"
	   else (ev (App sum0 (Con (read(head args))))) >> return ()
-}

runSimple :: Int -> String
runSimple n = show (simpleEval [] (App sum0 (Con n)))

runMonad :: Int -> String
runMonad n =
   let t = (App sum0 (Con n)) in
   let StateMonad2 m = traverseTerm t in
   let (env, t2) = m [] in
   pp t2 ++ "  " ++ ppenv env

------------------------------------------------------------
-- Data structures
------------------------------------------------------------
--instance Show (a -> b) where
--    show f = "<function>"


data Term
    = Var String
    | Con Int
    | Incr
    | Add Term Term
    | Lam String Term
    | App Term Term
    | IfZero Term Term Term
    -- the following terms are used internally
    | Thunk Term Env  -- a closure
    deriving (Eq,Show)

type Env = [(String,Term)]

{-
----------------------------------------------------------------------
-- Evaluate a term
----------------------------------------------------------------------
ev :: Term -> IO (Env,Term)
ev t =
    do  let StateMonad2 m = traverseTerm t
	let (env,t2) = m []
	putStrLn (pp t2 ++ "  " ++ ppenv env)
	return (env,t2)
-}

-----------------------------------------------------------------
-- This class extends Monad to have the standard features
-- we expect while evaluating/manipulating expressions.
----------------------------------------------------
class (Monad m) => EvalEnvMonad m where
    incr :: m ()     -- example of a state update function
    -- these defines the traversal!
    traverseTerm :: Term -> m Term
    --traversePred :: Pred -> m Pred
    lookupVar :: String -> m Term
    pushVar   :: String -> Term -> m a -> m a
    currEnv   :: m Env         -- returns the current environment
    withEnv   :: Env -> m a -> m a  -- uses the given environment
    pushVar v t m = do env <- currEnv; withEnv ((v,t):env) m


-- Here is a monad that evaluates the term.
newtype StateMonad2 a = StateMonad2 (Env -> (Env,a))

instance (Show a) => Show (StateMonad2 a) where
    show (StateMonad2 f) = show (f [])

instance Monad StateMonad2  where
    return a = StateMonad2 (\s -> (s,a))
    fail msg = StateMonad2 (\s -> (s,error' msg))
    (StateMonad2 g) >>= h =
	StateMonad2 (\a -> (let (s,a1) = g a in
			    (let StateMonad2 h' = h a1 in
			     h' s)))

instance EvalEnvMonad StateMonad2 where
    incr = StateMonad2 (\s -> (s,()))
    traverseTerm = eval
    lookupVar v =
	StateMonad2 (\env -> (env, lookup2 env))
	where
	lookup2 env = maybe (error' ("undefined var: " ++ v)) id (lookup v env)
    currEnv =
	StateMonad2 (\env -> (env,env))
    withEnv tmp (StateMonad2 m) =
	StateMonad2 (\env -> let (_,t) = m tmp in (env,t))


eval :: (EvalEnvMonad m) => Term -> m Term
eval (Var x)   =
    do e <- currEnv
       t <- lookupVar x
       traverseTerm t
eval (Add u v) =
    do {Con u' <- traverseTerm u;
	Con v' <- traverseTerm v;
	return (Con (u'+v'))}
eval (Thunk t e) =
    withEnv e (traverseTerm t)
eval f@(Lam x b) =
    do  env <- currEnv
	return (Thunk f env)  -- return a closure!
eval (App u v) =
    do {u' <- traverseTerm u;
	-- call-by-name, so we do not evaluate the argument v
	apply u' v
       }
eval (IfZero c a b) =
    do {val <- traverseTerm c;
	if val == Con 0
	   then traverseTerm a
	   else traverseTerm b}
eval (Con i)   = return (Con i)
eval (Incr)    = incr >> return (Con 0)

--apply :: Term -> Term -> StateMonad2 Term
apply (Thunk (Lam x b) e) a =
    do  orig <- currEnv
	withEnv e (pushVar x (Thunk a orig) (traverseTerm b))
apply a b         = fail ("bad application: " ++ pp a ++
			      "  [ " ++ pp b ++ " ].")




----------------------------------------------------------------------
-- A directly recursive Eval, with explicit environment
----------------------------------------------------------------------
-- A trivial monad so that we can use monad syntax.
data Id a = Id a

instance Monad Id where
    return t = Id t
    fail = error'
    (Id t) >>= f = f t

instance Show a => Show (Id a) where
    show (Id t) = show t

simpleEval :: Env -> Term -> Id Term
simpleEval env (Var v) =
    simpleEval env (maybe (error' ("undefined var: " ++ v)) id (lookup v env))
simpleEval env e@(Con _) =
    return e
simpleEval env e@Incr =
    return (Con 0)
simpleEval env (Add u v) =
    do {Con u' <- simpleEval env u;
	Con v' <- simpleEval env v;
	return (Con (u' + v'))}
    where
    addCons (Con a) (Con b) = return (Con (a+b))
    addCons (Con _) b = fail ("type error in second arg of Add: " ++ pp b)
    addCons a (Con _) = fail ("type error in first arg of Add: " ++ pp a)
simpleEval env f@(Lam x b) =
    return (Thunk f env)  -- return a closure!
simpleEval env (App u v) =
    do {u' <- simpleEval env u;
	-- call-by-name, so we do not evaluate the argument v
	simpleApply env u' v
       }
simpleEval env (IfZero c a b) =
    do {val <- simpleEval env c;
	if val == Con 0
	   then simpleEval env a
	   else simpleEval env b}
simpleEval env (Thunk t e) =
    simpleEval e t

simpleApply :: Env -> Term -> Term -> Id Term
simpleApply env (Thunk (Lam x b) e) a =
    simpleEval env2 b
    where
    env2 = (x, Thunk a env) : e
simpleApply env a b         = fail ("bad application: " ++ pp a ++
			      "  [ " ++ pp b ++ " ].")

------------------------------------------------------------
-- Utility functions for printing terms and envs.
------------------------------------------------------------
ppenv env = "[" ++ concatMap (\(v,t) -> v ++ "=" ++ pp t ++ ", ") env ++ "]"


pp :: Term -> String
pp = ppn 0

-- Precedences:
--   0 = Lam and If (contents never bracketed)
--   1 = Add
--   2 = App
--   3 = atomic and bracketed things
ppn :: Int -> Term -> String
ppn _ (Var v) = v
ppn _ (Con i) = show i
ppn _ (Incr)  = "INCR"
ppn n (Lam v t) = bracket n 0 ("@" ++ v ++ ". " ++ ppn (-1) t)
ppn n (Add a b) = bracket n 1 (ppn 1 a ++ " + " ++ ppn 1 b)
ppn n (App a b) = bracket n 2 (ppn 2 a ++ " " ++ ppn 2 b)
ppn n (IfZero c a b) = bracket n 0
    ("IF " ++ ppn 0 c ++ " THEN " ++ ppn 0 a ++ " ELSE " ++ ppn 0 b)
ppn n (Thunk t e) = bracket n 0 (ppn 3 t ++ "::" ++ ppenv e)

bracket outer this t | this <= outer = "(" ++ t ++ ")"
		     | otherwise     = t


------------------------------------------------------------
-- Test Data
------------------------------------------------------------
x  = (Var "x")
y  = (Var "y")
a1 = (Lam "x" (Add (Var "x") (Con 1)))
aa = (Lam "x" (Add (Var "x") (Var "x")))

-- These should all return 1
iftrue = (IfZero (Con 0) (Con 1) (Con 2))
iffalse = (IfZero (Con 1) (Con 2) (Con 1))

-- This function sums all the numbers from 0 upto its argument.
sum0 :: Term
sum0 = (App fix partialSum0)
partialSum0 = (Lam "sum"
		  (Lam "n"
		   (IfZero (Var "n")
		    (Con 0)
		    (Add (Var "n") (App (Var "sum") nMinus1)))))
nMinus1 = (Add (Var "n") (Con (-1)))

lfxx :: Term
lfxx = (Lam "x" (App (Var "F") (App (Var "x") (Var "x"))))

-- This is the fix point combinator:  Y
fix :: Term
fix = (Lam "F" (App lfxx lfxx))
