module Lambdachine.Id 
  ( module Lambdachine.Name
  , module Lambdachine.Id
  ) 
where

import Lambdachine.Utils
import Lambdachine.Name

data Id = Id
  { idName :: !Name
  , idDetails :: !IdDetails
  } deriving (Eq, Ord)

instance Show Id where
  show (Id n _) = show n

instance Uniquable Id where
  getUnique (Id n _) = getUnique n

data IdDetails
  = TopLevelId
  | LocalId
  | DataConId
  | PrimOpId
  | DataConInfoTableId
  deriving (Eq, Ord, Show)

mkTopLevelId :: Name -> Id
mkTopLevelId n = Id n TopLevelId

mkLocalId :: Name -> Id
mkLocalId n = Id n LocalId

mkDataConId :: Name -> Id
mkDataConId n = Id n DataConId

mkPrimOpId :: Name -> Id
mkPrimOpId n = Id n PrimOpId

mkDataConInfoTableId :: Name -> Id
mkDataConInfoTableId n = Id n DataConInfoTableId

isDataConId :: Id -> Bool
isDataConId ident = idDetails ident == DataConId

isLocalId :: Id -> Bool
isLocalId ident = idDetails ident == LocalId

isTopLevelId :: Id -> Bool
isTopLevelId ident = idDetails ident == TopLevelId

isPrimOpId :: Id -> Bool
isPrimOpId ident = idDetails ident == PrimOpId

isDataConInfoTableId :: Id -> Bool
isDataConInfoTableId (Id n DataConInfoTableId) = True
isDataConInfoTableId _ = False

instance Pretty IdDetails where
  ppr TopLevelId = text "gbl"
  ppr LocalId = text "lcl"
  ppr DataConId = text "dcon"
  ppr PrimOpId = text "prim"
  ppr DataConInfoTableId = text "con_info"

instance Pretty Id where
  ppr ident =
    let name = idName ident in
    case idDetails ident of
      TopLevelId -> gblcolour (ppr name) <> pale (char '_' <> ppr (getUnique name))
      PrimOpId -> ppr name
      LocalId -> varcolour (ppr name)
      DataConId -> dconcolour (ppr name)
      DataConInfoTableId -> dconcolour (ppr name) <> text "_con"
