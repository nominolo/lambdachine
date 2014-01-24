-- | Built-in types and constructors.
module Lambdachine.Builtin where

import Lambdachine.Utils.Pretty
import Lambdachine.Ghc.Utils
import Lambdachine.Id
import Lambdachine.Utils.Unique

import qualified TysWiredIn as Ghc ( trueDataConId, falseDataConId,
                                     trueDataCon, falseDataCon)

falseDataConId, trueDataConId :: GlobalEnv -> Id
falseItblId, trueItblId :: GlobalEnv -> Id
apDataConId :: GlobalEnv -> Id
blackholeDataConId :: GlobalEnv -> Id
updateItblId :: GlobalEnv -> Id
initCodeId :: GlobalEnv -> Id

falseDataConId env = toplevelId env undefined Ghc.falseDataConId
trueDataConId env = toplevelId env undefined Ghc.trueDataConId
falseItblId env = dataConInfoTableId env Ghc.falseDataCon
trueItblId env = dataConInfoTableId env Ghc.trueDataCon

apDataConId _env = mkDataConId (mkBuiltinName (mkBuiltinUnique 3) ".AP")
blackholeDataConId _env = mkDataConId (mkBuiltinName (mkBuiltinUnique 4) ".BLACKHOLE")
updateItblId _env = mkTopLevelId (mkBuiltinName (mkBuiltinUnique 5) ".UPDATE")
initCodeId _env = mkTopLevelId (mkBuiltinName (mkBuiltinUnique 6) "%lc_init")
