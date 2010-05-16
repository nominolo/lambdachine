module Main where

import Lambdachine.Utils
import Lambdachine.Ghc.CoreToGrin

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
    s <- newUniqueSupply
    putStrLn $ showPpr core_mdl
    pprint $ toGrinModule s core_mdl