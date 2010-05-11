module Lambdachine.Type where

import Data.List ( foldr1 )

newtype TyVar = TV String
  deriving (Eq, Show)

mkTyVar :: String -> TyVar
mkTyVar = TV

data TyCon = TyCon
  { tyConName :: String 
  , tyConArity :: Int
  } deriving (Eq, Ord, Show)

data Type
  = Var TyVar
  | App Type Type
  | ConApp TyCon [Type]
    -- ^ Constructor application.  (Saturated?)
  | Fun Type Type
    -- ^ Special variant of 'ConApp' for functions.
  | ForAll TyVar Type
  deriving (Eq, Show)

mkVarTy :: String -> Type
mkVarTy s = Var (TV s)

-- | Puts arrows between the list of types.
mkFun :: [Type] -> Type
mkFun [] = error "mkFun: Argument must be non-empty."
mkFun ts = foldr1 Fun ts

-- | Inverse of 'mkFun'.
viewFun :: Type -> [Type]
viewFun (Fun t1 t2) = t1 : viewFun t2
viewFun t = [t]

forAll :: [TyVar] -> Type -> Type
forAll [] ty = ty
forAll (v:vs) ty = ForAll v (forAll vs ty)

viewForAll :: Type -> ([TyVar], Type)
viewForAll = go []
 where
   go vs (ForAll v ty) = go (v:vs) ty
   go vs ty = (reverse vs, ty)

