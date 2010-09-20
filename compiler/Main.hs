{-# LANGUAGE PatternGuards #-}
module Main where

import Lambdachine.Utils
import Lambdachine.Ghc.Pipeline
import Lambdachine.Ghc.CoreToBC
--import Lambdachine.Grin.Eval
import Lambdachine.Grin.Bytecode
import Lambdachine.Grin.Analyse
import Lambdachine.Grin.RegAlloc
import Lambdachine.Interp.Exec

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
  (core_binds, data_tycons) <- compileToCore file
  liftIO $ do
    s <- newUniqueSupply 'g'
    putStrLn "================================================="
    --putStrLn $ showPpr core_binds
    putStrLn "-------------------------------------------------"
    let bcos = generateBytecode s core_binds data_tycons
    --putStrLn $ pretty bcos
    let bcos' = M.map allocRegs bcos
    pprint $ bcos'
    test_insts2 bcos'
    --let entry:_ = filter ((=="test") . show) (M.keys bcos')
    --pprint $ fst $ interp entry bcos'
