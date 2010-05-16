
module Lambdachine.Utils 
  ( module Lambdachine.Utils.Pretty
  , module Lambdachine.Utils.Unique
  , expectJust
  , second
  )
where

import Lambdachine.Utils.Pretty hiding ( (<$>) )
import Lambdachine.Utils.Unique

expectJust :: String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust msg _ = error msg

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)