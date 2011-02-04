module Lambdachine.Ghc.Utils where

import Lambdachine.Id as N
import Lambdachine.Utils.Unique hiding ( Uniquable(..) )

import qualified Id as Ghc
import qualified Name as Ghc
import qualified Module as Ghc
import qualified Outputable as Ghc
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
      mdl_str ++ "." ++ showSDocForUser alwaysQualify (Ghc.ppr (Ghc.getOccName name))
 where
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

-- | Take a GHC 'Unique.Unique' and turn it into a 'Unique'.
--
-- Be very careful when using this and make sure that the namespace
-- (the 'Char' argument to 'newUniqueSupply' and GHC's equivalent)
-- cannot possibly overlap.
fromGhcUnique :: Uniquable a => a -> Unique
fromGhcUnique x = fromExternalUnique (getKey (getUnique x))
