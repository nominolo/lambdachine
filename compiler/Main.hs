{-# LANGUAGE PatternGuards, CPP #-}
module Main where

#include "Locations.h"

import Lambdachine.Utils
import Lambdachine.Ghc.Pipeline
import Lambdachine.Ghc.CoreToBC
--import Lambdachine.Grin.Eval
import Lambdachine.Grin.Bytecode
import Lambdachine.Grin.Analyse
import Lambdachine.Grin.RegAlloc
--import Lambdachine.Interp.Exec
--import Lambdachine.Interp.Trace
import Lambdachine.Serialise
import qualified Lambdachine.Options as Cli

import Ghc.Api.Pipeline

import GHC
import HscTypes ( HscEnv(hsc_dflags) )
import DynFlags ( setPackageName, updOptLevel, dopt_unset )
import GHC.Paths ( libdir )
import Outputable
import MonadUtils ( liftIO )
import qualified Data.Map as M

import Control.Exception ( onException )
import Control.Monad ( when, unless )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs )
import System.Directory ( getTemporaryDirectory, renameFile, removeFile )
import System.IO ( openTempFile, hPutStr, hFlush, hClose )
import System.Cmd ( rawSystem )
import System.FilePath ( replaceExtension )
import System.IO.Temp

dbPath :: String
dbPath = PACKAGE_CONF_STRING

main :: IO ()
main = do
  opts <- Cli.getOptions
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    let dflags1a = dflags0{ ghcLink = NoLink
                          , hscTarget = HscAsm
--                          , verbosity = 5
                          , ghcMode = OneShot
                          -- , flags = Opt_D_dump_simpl_iterations :
                          --           Opt_D_verbose_core2core :
                          --           Opt_D_dump_rules :
                          --           Opt_D_dump_rule_firings :
                          --           Opt_D_dump_inlinings :
                          --           flags dflags0
                          }
        dflags1 = updOptLevel (Cli.optLevel opts) dflags1a
        dflags2 | Cli.package_name opts /= ""
                = setPackageName (Cli.package_name opts) dflags1
                | otherwise = dflags1
        dflags3 = dflags2{ systemPackageConfig = dbPath }
        dflags = dopt_unset dflags3 Opt_ReadUserPackageConf
    setSessionDynFlags dflags
    let file = Cli.inputFile opts
    hsc_env <- getSession

    let hooks = defaultPhaseImplementations
                  { runHsCompiler = compileToBytecode opts }
    _ <- compileFile hsc_env hooks StopLn (Source file Nothing)
    return ()
--  comp_result <- compileToCore file

hscRecompiled :: HscStatus
hscRecompiled = HscRecomp False ()

compileToBytecode :: Cli.Options -> HscEnv -> ModSummary -> Bool -> Ghc HscStatus
compileToBytecode options hsc_env mod_summary source_unchanged = do
  mb_bindings <- compileSingle hsc_env mod_summary source_unchanged
  case mb_bindings of
    Nothing ->
      return hscRecompiled
    Just (this_mod, core_binds, data_tycons, imports) -> liftIO $ do
      print (moduleNameString this_mod, map moduleNameString imports)
      s <- newUniqueSupply 'g'
      let !bcos = generateBytecode s this_mod core_binds data_tycons
      let !bco_mdl =
             allocRegs (moduleNameString this_mod)
                       (map moduleNameString imports)
                       bcos
      let file = ml_obj_file (ms_location mod_summary)
      let ofile = file `replaceExtension` ".lcbc"
      
      putStrLn $ "Writing bytecode to " ++ show ofile
      tmpdir <- getTemporaryDirectory
      (tmpfile, hdl) <- openBinaryTempFile tmpdir "lcc.lcbc"
      (`onException` (hClose hdl >> removeFile tmpfile)) $ do
        hWriteModule hdl bco_mdl
        hFlush hdl  -- just to be sure
        hClose hdl
        renameFile tmpfile ofile
      return hscRecompiled
{-

main :: IO ()
main = do
  opts <- Cli.getOptions
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    let dflags1a = dflags0{ ghcLink = NoLink }
--                          , ghcMode = OneShot }
        dflags1 = updOptLevel (Cli.optLevel opts) dflags1a
        dflags2 | Cli.package_name opts /= ""
                = setPackageName (Cli.package_name opts) dflags1
                | otherwise = dflags1
        dflags = dflags2{ systemPackageConfig = dbPath }
    setSessionDynFlags dflags
    let file = Cli.inputFile opts
    comp_result <- compileToCore file
    case comp_result of
     Nothing -> -- it was probably just a .hs-boot file
       return ()
     Just (this_mod, core_binds, data_tycons, imports) -> liftIO $ do
      print (moduleNameString this_mod,
            map moduleNameString imports)
      s <- newUniqueSupply 'g'
      when (Cli.dumpCoreBinds opts) $ do
        putStrLn "================================================="
        putStrLn $ showPpr core_binds

      let bcos = generateBytecode s this_mod core_binds data_tycons
      -- putStrLn $ pretty bcos
      let !bco_mdl =
            allocRegs (moduleNameString this_mod)
                      (map moduleNameString imports)
                      bcos

      when (Cli.dumpBytecode opts) $ do
        pprint $ bco_mdl

      let ofile = file `replaceExtension` ".lcbc"
--      putStrLn $ "Writing output to: " ++ show ofile

      tmpdir <- getTemporaryDirectory
      (tmpfile, hdl) <- openBinaryTempFile tmpdir "lcc.lcbc"
      (`onException` (hClose hdl >> removeFile tmpfile)) $ do
        hWriteModule hdl bco_mdl
        hFlush hdl  -- just to be sure
        hClose hdl
        renameFile tmpfile ofile

{-
--     tmp <- getTemporaryDirectory
--     (file, hdl) <- openTempFile tmp "trace.html"
--     hPutStr hdl (showHtml (defaultWrapper (toHtml (FLoad (TVar 3) 2))))
--     hFlush hdl
--     hClose hdl
--     _ <- rawSystem "open" [file]
--     return ()
-}
--    test_insts2 bcos'
   
    --let entry:_ = filter ((=="test") . show) (M.keys bcos')
    --pprint $ fst $ interp entry bcos'
    --test_record1 bcos'
-}
