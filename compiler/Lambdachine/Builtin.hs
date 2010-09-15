-- | Built-in types and constructors.
module Lambdachine.Builtin where

import Lambdachine.Utils.Unique
import Lambdachine.Id

falseDataConId :: Id
trueDataConId :: Id
apDataConId :: Id
blackholeDataConId :: Id

falseDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 1) "False")
trueDataConId  = mkDataConId (mkBuiltinName (mkBuiltinUnique 2) "True")
apDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 3) ".AP")
blackholeDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 4) ".BLACKHOLE")
