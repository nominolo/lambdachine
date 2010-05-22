-- | Built-in types and constructors.
module Lambdachine.Builtin where

import Lambdachine.Utils.Unique
import Lambdachine.Id

falseDataConId :: Id
trueDataConId :: Id

falseDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 1) "False")
trueDataConId = mkDataConId (mkBuiltinName (mkBuiltinUnique 2) "True")
