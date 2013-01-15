{-# LANGUAGE PatternGuards, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses #-}
module Lambdachine.Ghc.Pipeline where

import DynFlags
import HscTypes
import MonadUtils ( MonadIO(..) )
import CoreSyn
import CorePrep
import MkIface ( mkIfaceTc )
import GHC hiding ( exprType )
import HscMain ( hscSimplify, hscParse, hscTypecheck, hscDesugar,
                 hscWriteIface, hscNormalIface, hscGenHardCode )
import TcRnMonad ( TcGblEnv(..) )
import TidyPgm ( tidyProgram, mkBootModDetailsTc )
import SysTools ( touch )
import Outputable ( showPpr )
import TyCon ( isDataTyCon )
import Data.List ( find )
import Control.Monad ( liftM )
import qualified IdInfo as Ghc
import qualified Id as Ghc
import CoreUtils ( exprType )

import Data.Generics.Uniplate.Direct

io :: MonadIO m => IO a -> m a
io = liftIO

compileToCore :: GhcMonad m => FilePath
              -> m (Maybe (ModuleName, [CoreBind], [TyCon], [ModuleName]))
compileToCore file = do
  addTarget =<< guessTarget file Nothing
  _ <- load LoadAllTargets
  mod_graph <- depanal [] True
  case find ((== file) . msHsFilePath) mod_graph of
    Just mod_summary -> do
      withTempSession (\env ->
                         env{ hsc_dflags = ms_hspp_opts mod_summary }) $ do
        -- liftIO $ putStrLn "Parsing ..."
        rdr_module <- hscParse mod_summary
        -- liftIO $ putStrLn "Typechecking ..."
        tc_result <- hscTypecheck mod_summary rdr_module
        case ms_hsc_src mod_summary of
          HsBootFile -> do
            liftIO $ putStrLn "Writing .hi-boot file ..."
            (iface, changed, _) <- hscSimpleIface' tc_result
            hscWriteIface iface changed mod_summary
            return Nothing
          _ -> do
            -- liftIO $ putStrLn "Desugaring ..."
            guts <- hscDesugar mod_summary tc_result
            -- liftIO $ putStrLn "Optimising ..."
            guts' <- hscSimplify guts

            -- liftIO $ putStrLn "Translating to bytecode ..."            
            liftM Just (prepareCore mod_summary guts')
    Nothing -> 
      error $ "compileToCore: File not found in module graph: " ++ file

compileSingle :: GhcMonad m => HscEnv -> ModSummary -> Bool
              -> m (Maybe (ModuleName, [CoreBind], [TyCon], [ModuleName]))
compileSingle hsc_env mod_summary _source_unchanged = do
  withTempSession (\env ->
                     env{ hsc_dflags = ms_hspp_opts mod_summary }) $ do
    -- liftIO $ putStrLn "Parsing ..."
    rdr_module <- hscParse mod_summary
    -- liftIO $ putStrLn "Typechecking ..."
    tc_result <- hscTypecheck mod_summary rdr_module
    case ms_hsc_src mod_summary of
      HsBootFile -> do
        liftIO $ putStrLn "Writing .hi-boot file ..."
        (iface, changed, _) <- hscSimpleIface' tc_result
        hscWriteIface iface changed mod_summary
        return Nothing
      _ -> do
        liftIO $ putStrLn "Desugaring ..."
        guts <- hscDesugar mod_summary tc_result
        liftIO $ putStrLn "Optimising ..."
        guts' <- hscSimplify guts

        liftIO $ putStrLn "Writing iface"        
        (iface, changed, _details, cgguts) <- hscNormalIface guts' Nothing
        hscWriteIface iface changed mod_summary
        
        let dflags = hsc_dflags hsc_env
            s_file = hscOutName dflags
        liftIO $ SysTools.touch dflags "Touching .s file" s_file
--        _hasStub <- hscGenHardCode cgguts mod_summary

        liftIO $ putStrLn "Translating to bytecode ..."
        liftM Just (prepareCore mod_summary guts')

hscSimpleIface' :: GhcMonad m =>
                   TcGblEnv
                -> m (ModIface, Bool, ModDetails)
hscSimpleIface' tc_result = do
   hsc_env <- getSession
   details <- liftIO $ mkBootModDetailsTc hsc_env tc_result
   (new_iface, no_change)
       <- {-# SCC "MkFinalIface" #-}
          ioMsgMaybe $ mkIfaceTc hsc_env Nothing details tc_result
   -- And the answer is ...
   -- liftIO $ dumpIfaceStats hsc_env
   return (new_iface, no_change, details)
    

-- | Run simplifier and put Core into A-normal form.
prepareCore :: GhcMonad m => ModSummary -> ModGuts
            -> m (ModuleName, [CoreBind], [TyCon], [ModuleName])
prepareCore mod_summary mod_guts0 = do
  let this_mod = ms_mod_name mod_summary
  let imports =
        [ unLoc (ideclName imp)
        | L _ imp <- ms_imps mod_summary ++ ms_srcimps mod_summary ]
  simpl_guts <- hscSimplify mod_guts0
  hsc_env <- getSession
  (cg_guts, _details) <- io $ tidyProgram hsc_env simpl_guts
  let dflags = hsc_dflags hsc_env
      core_binds = cg_binds cg_guts
      data_tycons = filter isDataTyCon (cg_tycons cg_guts)
  binds0 <- io $ corePrepPgm dflags core_binds data_tycons
  let binds = map removeSpeculation binds0
  return (this_mod, binds, data_tycons, imports)

-- TODO: Move someplace else.  Separate package?

instance Uniplate CoreExpr where
  uniplate (App e1 e2)       = plate App |* e1 |* e2
  uniplate (Lam b e1)        = plate (Lam b) |* e1
  uniplate (Let b e1)        = plate Let |+ b |* e1
  uniplate (Case e b t alts) = plate Case |* e |- b |- t ||+ alts
  uniplate (Cast e c)        = plate Cast |* e |- c
  uniplate (Note n e)        = plate (Note n) |* e
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
        Ghc.PrimOpId _ <- Ghc.idDetails f
      = Case app x (exprType body) [(DEFAULT, [], body)]
    ubx_let_to_case e = e
