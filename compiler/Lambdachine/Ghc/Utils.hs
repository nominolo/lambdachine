{-# LANGUAGE CPP, PatternGuards #-}
module Lambdachine.Ghc.Utils where

import Lambdachine.Id as N
import Lambdachine.Utils.Unique hiding ( Uniquable(..) )
import Lambdachine.Grin.Bytecode as Grin
import Lambdachine.Utils.Pretty

import qualified TysWiredIn as Ghc
import qualified TysPrim as Ghc
import qualified TyCon as Ghc
import qualified TypeRep as Ghc
import qualified Id as Ghc
import qualified Name as Ghc
import qualified Module as Ghc
import qualified Outputable as Ghc
import qualified Type as Ghc
import qualified DataCon as Ghc
import Outputable ( Outputable, showPpr, alwaysQualify, showSDocForUser )
import Unique ( Uniquable(..), getKey )

-- | Directly turn GHC 'Ghc.Id' into 'Id'.
--
-- Reuses the 'Unique' from GHC.
toplevelId :: Ghc.ModuleName -> Ghc.Id -> Id
toplevelId mdl x = --  | Ghc.VanillaId <- Ghc.idDetails x =
  mkTopLevelId $
    N.mkBuiltinName (fromGhcUnique x) $
      mdl_str ++ "." ++ occ_part
 where
   occ_part0 = showSDocForUser alwaysQualify (Ghc.ppr (Ghc.getOccName name))
   occ_part
     -- FIXME: Dirty, dirty hack.
     | occ_part0 == "sat" = showSDocForUser alwaysQualify (Ghc.ppr name)
     | otherwise = occ_part0
   name = Ghc.getName x
   mdl_str
     | Just m <- Ghc.nameModule_maybe name
     = showSDocForUser alwaysQualify (Ghc.ppr (Ghc.moduleName m))
     | otherwise
     = showSDocForUser alwaysQualify (Ghc.ppr mdl)

dataConInfoTableId :: Ghc.DataCon -> Id
dataConInfoTableId dcon =
  mkDataConInfoTableId $
   N.mkBuiltinName (fromGhcUnique dcon)
      (showSDocForUser alwaysQualify (Ghc.ppr dcon))

splitFunTysN :: Int -> Ghc.Type -> Maybe ([Ghc.Type], Ghc.Type)
splitFunTysN n ty = split n ty []
 where
   split 0 ty acc = Just (reverse acc, ty)
   split n ty acc =
     case Ghc.splitFunTy_maybe ty of
       Nothing         -> Nothing
       Just (arg, ty') -> split (n - 1) ty' (arg:acc)

tyConId :: Ghc.Name -> Id
tyConId x =
  mkTopLevelId $
    N.mkBuiltinName (fromGhcUnique x)
      (showSDocForUser alwaysQualify (Ghc.ppr x))

-- | Take a GHC 'Unique.Unique' and turn it into a 'Unique'.
--
-- Be very careful when using this and make sure that the namespace
-- (the 'Char' argument to 'newUniqueSupply' and GHC's equivalent)
-- cannot possibly overlap.
fromGhcUnique :: Uniquable a => a -> Unique
fromGhcUnique x = fromExternalUnique (getKey (getUnique x))

ghcAnyTyCon :: Ghc.TyCon
ghcAnyType :: Ghc.Type

#if __GLASGOW_HASKELL__ >= 700
ghcAnyTyCon = Ghc.anyTyCon
ghcAnyType = Ghc.anyTypeOfKind Ghc.liftedTypeKind
#elif __GLASGOW_HASKELL__ >= 612
ghcAnyTyCon = Ghc.anyPrimTyCon
ghcAnyType = Ghc.anyPrimTy
#endif

-- | Translate a GHC System FC type into runtime type info.
--
-- We currently look through type abstraction and application.  A
-- polymorphic type (i.e., a type variable) is just represented as a
-- pointer.  At runtime such a value must have an associated info
-- table, so we can just look at that to figure out the type.
--
-- TODO: How to deal with 'void' types, like @State#@?
--
transType :: Ghc.Type -> OpTy
transType (Ghc.TyConApp tycon _)
  | Ghc.isPrimTyCon tycon =
    case () of
     _ | tycon == Ghc.intPrimTyCon   -> IntTy
       | tycon == Ghc.charPrimTyCon  -> CharTy
       | tycon == Ghc.floatPrimTyCon -> FloatTy
       | tycon == Ghc.byteArrayPrimTyCon -> PtrTy
       | tycon == ghcAnyTyCon           -> PtrTy
       | tycon == Ghc.bcoPrimTyCon       -> AddrTy
       | tycon == Ghc.addrPrimTyCon      -> AddrTy
       | tycon == Ghc.wordPrimTyCon  -> WordTy
       | tycon == Ghc.statePrimTyCon -> VoidTy
       | otherwise ->
         error $ "Unknown primitive type: " ++ showPpr tycon
  | otherwise =
    AlgTy (tyConId (Ghc.tyConName tycon))
transType ty@(Ghc.FunTy _ _) | (args, res) <- Ghc.splitFunTys ty =
  FunTy (map transType args) (transType res)
-- Type abstraction stuff.  See documentation above.
transType (Ghc.ForAllTy _ t) = transType t
transType (Ghc.TyVarTy _) = PtrTy
transType (Ghc.AppTy t _) = transType t
-- Get the dictionary data type for predicates.
-- TODO: I think this may cause a GHC panic under some circumstances.
transType (Ghc.PredTy pred) =
  transType (Ghc.predTypeRep pred)
transType ty =
  error $ "transType: Don't know how to translate type: "
          ++ showPpr ty

ghcPretty :: Ghc.Outputable a => a -> String
ghcPretty = Ghc.showSDoc . Ghc.ppr

