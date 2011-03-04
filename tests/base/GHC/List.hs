{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module GHC.List where

import GHC.Prim
import GHC.Tuple
import GHC.Base
import Control.Exception.Base

infixl 9  !!

head                    :: [a] -> a
head (x:_)              =  x
head []                 =  undef

tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  undef

undef = undef

last                    :: [a] -> a
last []                 =  undef
last (x:xs)             =  last' x xs
  where last' y []     = y
        last' _ (y:ys) = last' y ys

init                    :: [a] -> [a]
init []                 =  undef
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

length                  :: [a] -> Int
length l                =  len l 0#
  where
    len :: [a] -> Int# -> Int
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)

filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs

foldl        :: (a -> b -> a) -> a -> [b] -> a
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs

scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls            =  q : (case ls of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  undef

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs 

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []             =  []
scanr1 _ [x]            =  [x]
scanr1 f (x:xs)         =  f x q : qs
                           where qs@(q:_) = scanr1 f xs 

iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

repeat :: a -> [a]
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

cycle                   :: [a] -> [a]
cycle []                = undef
cycle xs                = xs' where xs' = xs ++ xs'

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

take                   :: Int -> [a] -> [a]
take (I# n#) xs = takeUInt n# xs

takeUInt :: Int# -> [b] -> [b]
takeUInt n xs
  | n >=# 0#  =  take_unsafe_UInt n xs
  | otherwise =  []

take_unsafe_UInt :: Int# -> [b] -> [b]
take_unsafe_UInt 0#  _  = []
take_unsafe_UInt m   ls =
  case ls of
    []     -> []
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

drop                   :: Int -> [a] -> [a]
drop (I# n#) ls
  | n# <# 0#    = ls
  | otherwise   = drop# n# ls
    where
        drop# :: Int# -> [a] -> [a]
        drop# 0# xs      = xs
        drop# _  xs@[]   = xs
        drop# m# (_:xs)  = drop# (m# -# 1#) xs

splitAt                :: Int -> [a] -> ([a],[a])
splitAt (I# n#) ls
  | n# <# 0#    = ([], ls)
  | otherwise   = splitAt# n# ls
    where
        splitAt# :: Int# -> [a] -> ([a], [a])
        splitAt# 0# xs     = ([], xs)
        splitAt# _  xs@[]  = (xs, xs)
        splitAt# m# (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt# (m# -# 1#) xs

span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

(!!)                    :: [a] -> Int -> a
xs !! (I# n0)
  | n0 <# 0# = undef
  | otherwise = sub xs n0
     where
       sub :: [a] -> Int# -> a
       sub []     _ = undef
       sub (y:ys) n = if n ==# 0#
                      then y
                      else sub ys (n -# 1#)
