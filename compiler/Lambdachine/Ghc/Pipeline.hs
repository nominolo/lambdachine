module Lambdachine.Ghc.Pipeline where

import HscTypes
import MonadUtils ( MonadIO(..) )
import CoreSyn
import CorePrep
import GHC
import HscMain ( hscSimplify, hscParse, hscTypecheck, hscDesugar )
import TidyPgm ( tidyProgram )
import TyCon ( isDataTyCon )
import Data.List ( find )

io :: MonadIO m => IO a -> m a
io = liftIO

compileToCore :: GhcMonad m => FilePath -> m ([CoreBind], [TyCon])
compileToCore file = do
  addTarget =<< guessTarget file Nothing
  _ <- load LoadAllTargets
  mod_graph <- depanal [] True
  case find ((== file) . msHsFilePath) mod_graph of
    Just mod_summary -> 
      withTempSession (\env -> env{ hsc_dflags = ms_hspp_opts mod_summary }) $
        hscParse mod_summary >>=
          hscTypecheck mod_summary >>=
            hscDesugar mod_summary >>=
              prepareCore mod_summary
    Nothing -> 
      error $ "compileToCore: File not found in module graph: " ++ file
    

-- | Run simplifier and put Core into A-normal form.
prepareCore :: GhcMonad m => ModSummary -> ModGuts
            -> m ([CoreBind], [TyCon])
prepareCore mod_summary mod_guts0 = do
  simpl_guts <- hscSimplify mod_guts0
  hsc_env <- getSession
  (cg_guts, _details) <- io $ tidyProgram hsc_env simpl_guts
  let dflags = hsc_dflags hsc_env
      core_binds = cg_binds cg_guts
      data_tycons = filter isDataTyCon (cg_tycons cg_guts)
  binds <- io $ corePrepPgm dflags core_binds data_tycons
  return (binds, data_tycons)

