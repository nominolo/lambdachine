-- | Built-in types and constructors.
module Lambdachine.Builtin where

import Lambdachine.Ghc.Utils
import Lambdachine.Id
import Lambdachine.Utils.Unique

import qualified TysWiredIn as Ghc ( trueDataConId, falseDataConId,
                                     trueDataCon, falseDataCon)

falseDataConId, trueDataConId :: Id
falseItblId, trueItblId :: Id
apDataConId :: Id
blackholeDataConId :: Id
updateItblId :: Id
initCodeId :: Id

falseDataConId = toplevelId undefined Ghc.falseDataConId
trueDataConId = toplevelId undefined Ghc.trueDataConId
falseItblId = dataConInfoTableId Ghc.falseDataCon
trueItblId  = dataConInfoTableId Ghc.trueDataCon

apDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 3) ".AP")
blackholeDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 4) ".BLACKHOLE")
updateItblId = mkTopLevelId (mkBuiltinName (mkBuiltinUnique 5) ".UPDATE")
initCodeId = mkTopLevelId (mkBuiltinName (mkBuiltinUnique 6) "%lc_init")
