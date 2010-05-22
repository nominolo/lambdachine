module Main where

import Lambdachine.Utils
import Lambdachine.Ghc.CoreToGrin
import Lambdachine.Grin.Eval

import GHC
import GHC.Paths ( libdir )
import Outputable
import MonadUtils ( liftIO )

import System.Environment ( getArgs )

main :: IO ()
main = runGhc (Just libdir) $ do
  [file] <- liftIO $ getArgs
  setSessionDynFlags =<< getSessionDynFlags
  core_mdl <- compileToCoreSimplified file
  liftIO $ do
    s <- newUniqueSupply 'g'
    putStrLn $ showPpr core_mdl
    let grin_mdl = toGrinModule s core_mdl
    pprint $ grin_mdl
    runTest1 grin_mdl
