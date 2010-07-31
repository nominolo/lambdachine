{-# LANGUAGE PatternGuards #-}
module Main where

import Lambdachine.Utils
import Lambdachine.Ghc.Pipeline
import Lambdachine.Ghc.CoreToBC
import Lambdachine.Grin.Eval
import Lambdachine.Grin.Bytecode

import GHC
import GHC.Paths ( libdir )
import Outputable
import MonadUtils ( liftIO )

import System.Environment ( getArgs )

main :: IO ()
main = runGhc (Just libdir) $ do
  args0 <- liftIO $ getArgs
  let file | [f] <- args0 = f
           | otherwise = "../tests/bc0001.hs"
  setSessionDynFlags =<< getSessionDynFlags
  core_mdl <- compileToCore file
  liftIO $ do
    s <- newUniqueSupply 'g'
    putStrLn "================================================="
    putStrLn $ showPpr core_mdl
    putStrLn "-------------------------------------------------"
--    putStrLn $ showPpr (head core_mdl)
    let bcos = generateBytecode s core_mdl
    putStrLn $ pretty bcos
--    let bc = runTrans s $ bcBindTopLvl core_mdl
--    putStrLn $ pretty bc
    --let grin_mdl = toGrinModule s core_mdl
    --pprint $ grin_mdl
    --s2 <- newUniqueSupply 'b'
--    putStrLn $ pretty $ toBytecode s2 grin_mdl
    --runTest1 grin_mdl
