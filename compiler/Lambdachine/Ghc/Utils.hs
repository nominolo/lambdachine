{-# LANGUAGE CPP, PatternGuards #-}
module Lambdachine.Ghc.Utils where

import Lambdachine.Id as N
import Lambdachine.Utils hiding ( Uniquable(..) )
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
import qualified CoreSyn as Ghc
import qualified CoreFVs as Ghc
import qualified Var as Ghc
import qualified VarSet as Ghc
import DynFlags ( tracingDynFlags )
import Outputable ( Outputable, alwaysQualify )
import qualified Outputable ( showPpr, showSDocForUser )
import Unique ( Uniquable(..), getKey )

showPpr :: Outputable a => a -> String
showPpr = Outputable.showPpr tracingDynFlags

showSDocForUser :: Ghc.PrintUnqualified -> Ghc.SDoc -> String
showSDocForUser = Outputable.showSDocForUser tracingDynFlags

ghcPretty :: Ghc.Outputable a => a -> String
ghcPretty = Ghc.showSDoc tracingDynFlags . Ghc.ppr

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

isGhcVoid :: Ghc.CoreBndr -> Bool
isGhcVoid x = isGhcVoidType (Ghc.varType x)

isGhcVoidType :: Ghc.Type -> Bool
--isGhcVoidType ty = transType (Ghc.repType ty) == VoidTy
isGhcVoidType ty = transType ty == VoidTy

-- | Split unboxed tuples into their non-void components.  Leave
-- everything else untouched.
--
-- > { (# a, b, c #) }  ~~>  [{ a }, { b }, { c }]
-- > { (# State# s, Int #) }  ~~>  [{ Int }]
-- > { State# s }  ~~>  [{ State# s #}]
-- > { Maybe Int }  ~~>  [{ Maybe Int }]
-- > { Char }  ~~>  [{ Char }]
splitUnboxedTuples :: Ghc.Type -> [Ghc.Type]
splitUnboxedTuples ty = case Ghc.splitTyConApp_maybe ty of
  Just (tc, args)
    | Ghc.isUnboxedTupleTyCon tc -> removeIf isGhcVoidType args
  _ -> [ty]

isFreeExprVarIn :: Ghc.Id -> Ghc.CoreExpr -> Bool
isFreeExprVarIn x expr =
  Ghc.elemVarSet x (ghcFreeExprVars expr)

-- | Return all free variables of the expression (excluding free type variables).
ghcFreeExprVars :: Ghc.CoreExpr -> Ghc.IdSet
ghcFreeExprVars = Ghc.exprFreeIds

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
transType ty0 = case Ghc.repType ty0 of
  Ghc.UnaryRep rep_ty ->
    case Ghc.tyConAppTyCon_maybe rep_ty of
      Just tycon
        | Ghc.isUnLiftedTyCon tycon
        -> case () of
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

        | Ghc.isAbstractTyCon tycon
        -> PtrTy

        | Ghc.isAlgTyCon tycon
        -> AlgTy (tyConId (Ghc.tyConName tycon))

        | otherwise
        -> if not (Ghc.isFunTyCon tycon || Ghc.isPrimTyCon tycon || Ghc.isFamilyTyCon tycon)
             then error $ "Unexpected tycon" ++ showPpr tycon
             else PtrTy

{-

transType :: Ghc.Type -> OpTy
transType ty0 = transType1 (Ghc.expandTypeSynonyms ty0)

transType1 :: Ghc.Type -> OpTy
transType1 (Ghc.TyConApp tycon _)
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
transType1 ty@(Ghc.FunTy _ _) | (args, res) <- Ghc.splitFunTys ty =
  FunTy (map transType1 args) (transType1 res)
-- Type abstraction stuff.  See documentation above.
transType1 (Ghc.ForAllTy _ t) = transType1 t
transType1 (Ghc.TyVarTy _) = PtrTy
transType1 (Ghc.AppTy t _) = transType1 t
-- Get the dictionary data type for predicates.
-- TODO: I think this may cause a GHC panic under some circumstances.
transType1 (Ghc.PredTy pred) =
  transType1 (Ghc.predTypeRep pred)
transType1 ty =
  error $ "transType1: Don't know how to translate type: "
          ++ showPpr ty
-}

