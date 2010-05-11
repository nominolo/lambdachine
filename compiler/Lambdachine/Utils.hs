module Lambdachine.Utils 
  ( module Lambdachine.Utils.Pretty
  , module Lambdachine.Utils.Unique
  , expectJust
  )
where

import Lambdachine.Utils.Pretty hiding ( (<$>) )
import Lambdachine.Utils.Unique

expectJust :: String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust msg _ = error msg
