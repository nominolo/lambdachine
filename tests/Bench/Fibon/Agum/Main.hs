{-# LANGUAGE NoImplicitPrelude #-}
module Bench.Fibon.Agum.Main where

import GHC.Base
import GHC.Show
import Text.Read
import Data.List
--import System.IO (isEOF, hFlush, stdout)
import Bench.Fibon.Agum.UnificationMatching

testE :: String -> String
testE prob =
  case readM prob of
    Err err -> "ERROR: " ++ err
    Ans (Equation (t0, t1)) ->
      unlines 
        [ "Problem:   " ++ show (Equation (t0, t1))
        , "Unifier:   " ++ show (unify (Equation (t0, t1)))
        , "Matcher:   " ++ (case match (Equation (t0, t1)) of
                              Err err -> err
                              Ans subst -> show subst)
        ]

test = testE "2x + y = 3z" == "Problem:   2x + y = 3z\nUnifier:   [x : g0,y : -2g0 + 3g2,z : g2]\nMatcher:   [x : g0,y : -2g0 + 3z]\n"

{-
-- Given an equation, display a unifier and a matcher.
test1 :: String -> IO ()
test1 prob =
    case readM prob of
      Err err -> putStrLn err
      Ans (Equation (t0, t1)) ->
          do
            putStr "Problem:   "
            print $ Equation (t0, t1)
            let subst = unify $ Equation (t0, t1)
            putStr "Unifier:   "
            print subst
            putStr "Matcher:   "
            case match $ Equation (t0, t1) of
              Err err -> putStrLn err
              Ans subst -> print subst
            putStrLn ""
-}
readM :: (Read a, Monad m) => String -> m a
readM s =
    case [ x | (x, t) <- reads s, ("", "") <- lex t ] of
      [x] -> return x
      [] -> fail "no parse"
      _ -> fail "ambiguous parse"

data AnsErr a
    = Ans a
    | Err String

instance Monad AnsErr where
    (Ans x) >>= k = k x
    (Err s) >>= _ = Err s
    return        = Ans
    fail          = Err

-- Main loop

{-
main :: IO ()
main =
    do
      putStrLn "Abelian group unification and matching -- :? for help"
      loop1

loop1 :: IO ()
loop1 =
    do
      putStr "agum> "
      hFlush stdout
      done <- isEOF
      case done of
        True ->
            do
              putStrLn ""
              return ()
        False ->
            do
              line <- getLine
              case () of
                _ | line == ":?" || line == ":help" ->
                      do
                        help
                        loop1
                  | line == ":quit" ->
                      return ()
                  | otherwise ->
                      do
                        test1 line
                        loop1

help :: IO ()
help =
    mapM_ putStrLn mesg

mesg :: [String]
mesg =
    [ "Pose a question as an equation such as",
      "    2x + y = 3z, or",
      "    2x = x + y, or",
      "    64x - 41y = a.",
      "The agum programs shows the result of unification and matching.",
      "",
      "The unification problem is given two terms t and t', find a most",
      "general unifier s such that s(t) = s(t').  The matching problem",
      "for terms t and t' is to find a most general matcher s such that",
      "s(t) = t'.",
      "",
      ":quit quits the program, :? and :help print this message."]
-}