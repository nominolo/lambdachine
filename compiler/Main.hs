{-# LANGUAGE PatternGuards #-}
module Main where

import Lambdachine.Utils
import Lambdachine.Ghc.Pipeline
import Lambdachine.Ghc.CoreToBC
import Lambdachine.Grin.Eval
import Lambdachine.Grin.Bytecode
import Lambdachine.Grin.Analyse
import Lambdachine.Grin.RegAlloc

import GHC
import GHC.Paths ( libdir )
import Outputable
import MonadUtils ( liftIO )
import qualified Data.Map as M

import System.Environment ( getArgs )

main :: IO ()
main = runGhc (Just libdir) $ do
  args0 <- liftIO $ getArgs
  let file | [f] <- args0 = f
           | otherwise = "../tests/bc0001.hs"
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags{ ghcLink = NoLink }
  core_mdl <- compileToCore file
  liftIO $ do
    s <- newUniqueSupply 'g'
    putStrLn "================================================="
    putStrLn $ showPpr core_mdl
    putStrLn "-------------------------------------------------"
    let bcos = generateBytecode s core_mdl
    putStrLn $ pretty bcos
    let bcos' = M.map allocRegs bcos
    pprint $ bcos'
