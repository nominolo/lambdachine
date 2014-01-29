{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards, CPP #-}
module Main where

#include "Locations.h"

import Lambdachine.Utils
--import Lambdachine.Ghc.Pipeline
--import Lambdachine.Ghc.CoreToBC
import Lambdachine.Ghc.StgToBytecode
--import Lambdachine.Grin.Eval
import Lambdachine.Grin.Bytecode
import Lambdachine.Grin.Analyse
import Lambdachine.Grin.RegAlloc hiding ( allocRegs )
import Lambdachine.Grin.RegAllocLinearScan
import Lambdachine.Ghc.Utils
--import Lambdachine.Interp.Exec
--import Lambdachine.Interp.Trace
import Lambdachine.Serialise
import qualified Lambdachine.Options as Cli

#if __GLASGOW_HASKELL__ == 706
import Ghc.Api.V76
import Ghc.Api.V76Hsc
#elif __GLASGOW_HASKELL__ >= 707 && __GLASGOW_HASKELL__ < 709
import DriverPipeline -- ( runPhase )
import Hooks
import DriverPhases ( Phase(..) )
#else
#error "This version of GHC is not supported. Sorry."
#endif

import ErrUtils ( Messages )
import GHC
import HscTypes ( HscEnv(hsc_dflags), CgGuts(..), HscStatus(..) )
import DynFlags ( setPackageName, updOptLevel, dopt_unset, DynFlags(..), Settings(..), PkgConfRef(..) )
import GHC.Paths ( libdir )
import Outputable hiding ( showPpr )
import qualified Outputable as Out
import MonadUtils ( liftIO )
import Bag ( emptyBag )
import qualified Data.Map as M
import CoreSyn as Ghc
import CoreUtils as Ghc
import CorePrep
import TyCon ( isDataTyCon )
import qualified Id as Ghc
import CoreToStg        ( coreToStg )
import SimplStg         ( stg2stg )
import qualified SysTools

import Data.Generics.Uniplate.Direct


import Control.Exception ( onException )
import Control.Monad ( when, unless )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs )
import System.Directory ( getTemporaryDirectory, renameFile, removeFile, createDirectoryIfMissing )
import System.IO ( openTempFile, hPutStr, hFlush, hClose )
--import System.Cmd ( rawSystem )
import System.FilePath ( replaceExtension, takeDirectory )
import System.IO.Temp
import System.Exit

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
        dflags3 = dflags2{ settings = (settings dflags2){ sSystemPackageConfig = dbPath }}

        isNotUser UserPkgConf = False
        isNotUser _ = True

        dflags4 = dflags3{ extraPkgConfs = filter isNotUser . extraPkgConfs dflags3 }

        dflags = dflags4{ hooks = (hooks dflags4){ runPhaseHook = Just (lcPhaseHook opts) } }

        --dopt_unset dflags3 Opt_ReadUserPackageConf
    setSessionDynFlags dflags
    let file = Cli.inputFile opts
    hsc_env <- getSession

    out <- handleSourceError (\e -> do
               GHC.printException e
               liftIO $ exitWith (ExitFailure 1)) $ 
             liftIO $ compileFile hsc_env StopLn (file, Nothing)
    return ()


lcPhaseHook :: Cli.Options -> PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath)
lcPhaseHook options (HscOut src_flavour mod_name result) _ dflags = do
  -- Copied from GHC's DriverPipeline implementation
  location <- getLocation src_flavour mod_name
  setModLocation location

  let o_file = ml_obj_file location -- The real object file
      hsc_lang = hscTarget dflags
      next_phase = hscPostBackendPhase dflags src_flavour hsc_lang

  case result of
    HscNotGeneratingCode ->
      return (RealPhase StopLn,
              panic "No output filename from Hsc when no-code")
    HscUpToDate ->
        do liftIO $ touchObjectFile dflags o_file
           -- The .o file must have a later modification date
           -- than the source file (else we wouldn't get Nothing)
           -- but we touch it anyway, to keep 'make' happy (we think).
           return (RealPhase StopLn, o_file)
    HscUpdateBoot ->
        do -- In the case of hs-boot files, generate a dummy .o-boot
           -- stamp file for the benefit of Make
           liftIO $ touchObjectFile dflags o_file
           return (RealPhase next_phase, o_file)
    HscRecomp cgguts mod_summary
      -> do output_fn <- phaseOutputFilename next_phase

            PipeState{hsc_env=hsc_env'} <- getPipeState

            (outputFilename, mStub) <- liftIO $ genBytecode options hsc_env' cgguts mod_summary output_fn

            -- TODO: We don't support stubs yet
            -- case mStub of
            --   Nothing -> return ()
            --   Just stub_c -> do
            --     stub_o <- liftIO $ compileStub hsc_env' stub_c
            --     setStubO stub_o

            -- TODO: No linking at this point
            return (RealPhase StopLn, outputFilename)

lcPhaseHook _options pp path dflags = do
  --liftIO $ putStrLn $ Out.showPpr dflags pp
  runPhase pp path dflags



genBytecode :: Cli.Options -> HscEnv -> CgGuts -> ModSummary -> FilePath -> IO (FilePath, ())
genBytecode options hsc_env guts mod_summary outputFilename = do
  
  let dflags = hsc_dflags hsc_env
  let CgGuts{ -- This is the last use of the ModGuts in a compilation.
              -- From now on, we just use the bits we need.
        cg_module   = this_mod,
        cg_binds    = core_binds,
        cg_tycons   = tycons,
        cg_foreign  = foreign_stubs0,
        cg_dep_pkgs = dependencies,
        cg_hpc_info = hpc_info } = guts
                                   
  let imports =
        [ unLoc (ideclName imp)
        | L _ imp <- ms_textual_imps mod_summary ++ ms_srcimps mod_summary ]

  let data_tycons = filter isDataTyCon tycons

  prepd_binds <- corePrepPgm dflags hsc_env core_binds data_tycons

  stg_binds <- coreToStg dflags this_mod prepd_binds

  (stg_binds2, cost_centre_info) <- stg2stg dflags this_mod stg_binds
  let stg_binds3 = stg_binds2

  when (Cli.dumpCoreBinds options) $ do
    putStrLn "================================================="
    putStrLn $ Out.showPpr dflags stg_binds2

  s <- newUniqueSupply 'g'

  let genv = GlobalEnv dflags
  let bcos = stgToBytecode genv s (moduleName this_mod) stg_binds3 data_tycons

  when (Cli.dumpBytecode options) $ do
    pprint genv bcos

  let !bco_mdl =
         allocRegs genv
                   (moduleNameString (moduleName this_mod))
                   (map moduleNameString imports)
                   bcos

  when (Cli.dumpBytecode options) $ do
    pprint genv bco_mdl
  
  let file = ml_obj_file (ms_location mod_summary)
  let ofile = file `replaceExtension` ".lcbc"
  
  putStrLn $ "Writing bytecode to " ++ show ofile
  tmpdir <- getTemporaryDirectory
  (tmpfile, hdl) <- openBinaryTempFile tmpdir "lcc.lcbc"
  (`onException` (hClose hdl >> removeFile tmpfile)) $ do
    hWriteModule genv hdl bco_mdl
    hFlush hdl  -- just to be sure
    hClose hdl
    renameFile tmpfile ofile

  return (ofile, ())


touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path


{-    
    let hooks = defaultHooks
                  { hookCodeGen = compileToBytecode2 opts hsc_env
                  , hookPostBackendPhase =
                      \_default _dflags _hssourc _target ->
                         StopLn }

    out <- liftIO $ compileFile hsc_env hooks StopLn(file, Nothing)
    return ()
-}
    -- let hooks = defaultPhaseImplementations
    --               { runHsCompiler = compileToBytecode opts }
    -- _ <- compileFile hsc_env hooks StopLn (Source file Nothing)
    -- return ()
--  comp_result <- compileToCore file

-- hscRecompiled :: HscStatus
-- hscRecompiled = HscRecomp False ()
{-
compileToBytecode' :: Cli.Options
                   -> HscEnv
                   -> Hook (ModIface -> ModDetails -> CgGuts -> ModSummary
                                -> IO (Messages, Maybe (Maybe FilePath)))
                   -- -> ModIface -> ModDetails -> CgGuts -> ModSummary
                   -- -> IO (Messages, Maybe FilePath)
compileToBytecode' options hsc_env _default modIface modDetails guts mod_summary = do
  
  let dflags = hsc_dflags hsc_env
  -- guts are already tidy
  let core_binds0 = cg_binds guts
      data_tycons = filter isDataTyCon $ cg_tycons guts
      this_mod = moduleName (cg_module guts) :: ModuleName
      imports =
        [ unLoc (ideclName imp)
        | L _ imp <- ms_textual_imps mod_summary ++ ms_srcimps mod_summary ]

  -- but they're not in CorePrep form
  core_binds1 <- liftIO $ corePrepPgm dflags hsc_env core_binds0 data_tycons
  let core_binds = map removeSpeculation core_binds1

  do
      print (moduleNameString this_mod, map moduleNameString imports)
      s <- newUniqueSupply 'g'

      when (Cli.dumpCoreBinds options) $ do
        putStrLn "================================================="
        putStrLn $ ghcPretty core_binds

      let !bcos = generateBytecode s this_mod core_binds data_tycons

      when (Cli.dumpBytecode options) $ do
        pprint $ bcos

      let !bco_mdl =
             allocRegs (moduleNameString this_mod)
                       (map moduleNameString imports)
                       bcos

      when (Cli.dumpBytecode options) $ do
        pprint $ bco_mdl

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
      return ((emptyBag, emptyBag), Just Nothing)
  

compileToBytecode2 :: Cli.Options
                   -> HscEnv
                   -> Hook (ModIface -> ModDetails -> CgGuts -> ModSummary
                                -> IO (Messages, Maybe (Maybe FilePath)))
                   -- -> ModIface -> ModDetails -> CgGuts -> ModSummary
                   -- -> IO (Messages, Maybe FilePath)
compileToBytecode2 options hsc_env _default modIface modDetails guts mod_summary = liftIO $ do

  let dflags = hsc_dflags hsc_env
  let CgGuts{ -- This is the last use of the ModGuts in a compilation.
              -- From now on, we just use the bits we need.
        cg_module   = this_mod,
        cg_binds    = core_binds,
        cg_tycons   = tycons,
        cg_foreign  = foreign_stubs0,
        cg_dep_pkgs = dependencies,
        cg_hpc_info = hpc_info } = guts
                                   
  let imports =
        [ unLoc (ideclName imp)
        | L _ imp <- ms_textual_imps mod_summary ++ ms_srcimps mod_summary ]

  let data_tycons = filter isDataTyCon tycons

  prepd_binds <- corePrepPgm dflags hsc_env core_binds data_tycons

  stg_binds <- coreToStg dflags prepd_binds

  (stg_binds2, cost_centre_info) <- stg2stg dflags this_mod stg_binds
  let (stg_binds3, _) = unzip stg_binds2

  when (Cli.dumpCoreBinds options) $ do
    putStrLn "================================================="
    putStrLn $ showPpr stg_binds2

  s <- newUniqueSupply 'g'

  let bcos = stgToBytecode s (moduleName this_mod) stg_binds3 data_tycons

  when (Cli.dumpBytecode options) $ do
    pprint $ bcos

  let !bco_mdl =
         allocRegs (moduleNameString (moduleName this_mod))
                   (map moduleNameString imports)
                   bcos

  when (Cli.dumpBytecode options) $ do
    pprint $ bco_mdl
  
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
  return ((emptyBag, emptyBag), Just Nothing)

{-
compileToBytecode :: Cli.Options -> HscEnv -> ModSummary -> Bool -> Ghc HscStatus
compileToBytecode options hsc_env mod_summary source_unchanged = do
  mb_bindings <- compileSingle hsc_env mod_summary source_unchanged
  case mb_bindings of
    Nothing ->
      return hscRecompiled
    Just (this_mod, core_binds, data_tycons, imports) -> liftIO $ do
      print (moduleNameString this_mod, map moduleNameString imports)
      s <- newUniqueSupply 'g'

      when (Cli.dumpCoreBinds options) $ do
        putStrLn "================================================="
        putStrLn $ showPpr core_binds

      let !bcos = generateBytecode s this_mod core_binds data_tycons

      when (Cli.dumpBytecode options) $ do
        pprint $ bcos

      let !bco_mdl =
             allocRegs (moduleNameString this_mod)
                       (map moduleNameString imports)
                       bcos

      when (Cli.dumpBytecode options) $ do
        pprint $ bco_mdl

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
-}

------------------------------------------------------------------------



instance Uniplate CoreExpr where
  uniplate (App e1 e2)       = plate App |* e1 |* e2
  uniplate (Lam b e1)        = plate (Lam b) |* e1
  uniplate (Let b e1)        = plate Let |+ b |* e1
  uniplate (Ghc.Case e b t alts) = plate Ghc.Case |* e |- b |- t ||+ alts
  uniplate (Cast e c)        = plate Cast |* e |- c
  uniplate (Tick t e)        = plate (Tick t) |* e
  uniplate e                 = plate e

instance Biplate CoreExpr CoreExpr where biplate = plateSelf

instance Biplate CoreAlt CoreExpr where
  biplate (con, bs, e) = plate ((,,) con bs) |* e

instance Biplate CoreBind CoreExpr where
  biplate (NonRec x e) = plate (NonRec x) |* e
  biplate (Rec bs) = plate Rec ||+ bs

instance Biplate (CoreBndr, CoreExpr) CoreExpr where
  biplate (x, e) = plate ((,) x) |* e

removeSpeculation :: CoreBind -> CoreBind
removeSpeculation bind = transformBi ubx_let_to_case bind
  where
    ubx_let_to_case :: CoreExpr -> CoreExpr
    --ubx_let_to_case e | trace ("ubx: " ++ showPpr e) False = undefined
    ubx_let_to_case (Let (NonRec x app@(App _ _)) body)
      | (Var f, args) <- collectArgs app,
        Ghc.isPrimOpId f
        -- Ghc.PrimOpId _ <- Ghc.idDetails f
      = Ghc.Case app x (Ghc.exprType body) [(DEFAULT, [], body)]
    ubx_let_to_case e = e


------------------------------------------------------------------------




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
-}
