{-# LANGUAGE BangPatterns #-}
module Lambdachine.Utils 
  ( module Lambdachine.Utils.Pretty
  , module Lambdachine.Utils.Unique
  , expectJust
  , second
  , modify'
  -- * List Utilities
  , fold2l', isLength
  )
where

import Lambdachine.Utils.Pretty
import Lambdachine.Utils.Unique

import Control.Monad.State.Strict
import Data.Maybe

-- | Like 'fromJust' but uses the given string as error message.
expectJust :: String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust msg _ = error $ "expectJust: " ++ msg

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

-- | Variant of 'modify' that is strict in the state.
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do s <- get; put $! f s

-- | Strict left fold over two lists.  Returns after fully processing
-- the shortest input list.  Strict in all arguments.
--
-- > fold2l' (\x y z -> x + y * z) 0 [1..3] [5..] == 38
-- > 0 + 1 * 5 + 2 * 6 + 3 * 7 == 38
fold2l' :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
fold2l' f a0 bs0 cs0 = go a0 bs0 cs0
 where go !a [] _ = a
       go !a _ [] = a
       go !a (b:bs) (c:cs) = go (f a b c) bs cs

-- | @isLength n xs@ is a lazy equivalent of @length xs == n@.  It
-- is lazy in the sense that it evaluates at most @n@ elements of the
-- spine of the list.  Examples:
--
-- > isLength 3 [1..3] == True
-- > isLength 3 [1..2] == False
-- > isLength 3 [1..]  == False
-- > isLength 3 (cycle [42]) == False
-- > isLength 2 [undefined, undefined] == True
--
isLength :: Int -> [a] -> Bool
isLength n _  | n < 0 = False
isLength n []         = n == 0
isLength n (_:xs)     = isLength (n - 1) xs
