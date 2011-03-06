{-# LANGUAGE PatternGuards, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses #-}
module Lambdachine.Ghc.Pipeline where

import DynFlags
import HscTypes
import MonadUtils ( MonadIO(..) )
import CoreSyn
import CorePrep
import GHC hiding ( exprType )
import HscMain ( hscSimplify, hscParse, hscTypecheck, hscDesugar )
import TidyPgm ( tidyProgram )
import TyCon ( isDataTyCon )
import Data.List ( find )
import qualified IdInfo as Ghc
import qualified Id as Ghc
import CoreUtils ( exprType )

import Data.Generics.Uniplate.Direct

io :: MonadIO m => IO a -> m a
io = liftIO

compileToCore :: GhcMonad m => FilePath
              -> m (ModuleName, [CoreBind], [TyCon], [ModuleName])
compileToCore file = do
  addTarget =<< guessTarget file Nothing
  _ <- load LoadAllTargets
  mod_graph <- depanal [] True
  case find ((== file) . msHsFilePath) mod_graph of
    Just mod_summary -> 
      withTempSession (\env ->
                         env{ hsc_dflags = updOptLevel 0 $ ms_hspp_opts mod_summary }) $
        hscParse mod_summary >>=
          hscTypecheck mod_summary >>=
            hscDesugar mod_summary >>=
              hscSimplify >>=
                prepareCore mod_summary
    Nothing -> 
      error $ "compileToCore: File not found in module graph: " ++ file
    

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
