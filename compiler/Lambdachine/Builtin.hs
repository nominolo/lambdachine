-- | Built-in types and constructors.
module Lambdachine.Builtin where

import Lambdachine.Utils.Unique
import Lambdachine.Id


falseDataConName, trueDataConName :: Name
falseDataConId, trueDataConId :: Id
falseItblId, trueItblId :: Id
apDataConId :: Id
blackholeDataConId :: Id
updateItblId :: Id
initCodeId :: Id

falseDataConName = mkBuiltinName (mkBuiltinUnique 1) "False"
trueDataConName  = mkBuiltinName (mkBuiltinUnique 2) "True"
falseDataConId = mkDataConId falseDataConName
trueDataConId  = mkDataConId trueDataConName
falseItblId = mkDataConInfoTableId falseDataConName
trueItblId  = mkDataConInfoTableId trueDataConName

apDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 3) ".AP")
blackholeDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 4) ".BLACKHOLE")
updateItblId = mkTopLevelId (mkBuiltinName (mkBuiltinUnique 5) ".UPDATE")
initCodeId = mkTopLevelId (mkBuiltinName (mkBuiltinUnique 6) "%lc_init")