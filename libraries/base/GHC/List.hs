{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module GHC.List (

   map, (++), filter, concat,
   head, last, tail, init, null, length, (!!),
   foldl, scanl, scanl1, foldr, foldr1, scanr, scanr1,
   iterate, repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break,
   reverse, and, or,
   any, all, elem, notElem, lookup,
   concatMap,
   zip, zip3, zipWith, zipWith3, unzip, unzip3,
   errorEmptyList,
) where

import GHC.Prim
import GHC.Tuple
import GHC.Base
import Data.Maybe
-- import Control.Exception.Base

infixl 9  !!
infix  4 `elem`, `notElem`

head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead

badHead :: a
badHead = errorEmptyList "head"
-- This rule is useful in cases like 
--      head [y | (x,y) <- ps, x==t]
{-# RULES
"head/build"    forall (g::forall b.(a->b->b)->b->b) .
                head (build g) = g (\x _ -> x) badHead
"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) . 
                head (augment g xs) = g (\x _ -> x) (head xs)
 #-}

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

iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x = x `c` iterateFB c f (f x)

{-# RULES
"iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
"iterateFB"  [1]                iterateFB (:) = iterate
 #-}

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
  | isTrue# (n >=# 0#)  =  take_unsafe_UInt n xs
  | otherwise           =  []

take_unsafe_UInt :: Int# -> [b] -> [b]
take_unsafe_UInt 0#  _  = []
take_unsafe_UInt m   ls =
  case ls of
    []     -> []
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

drop                   :: Int -> [a] -> [a]
drop (I# n#) ls
  | isTrue# (n# <# 0#)    = ls
  | otherwise   = drop# n# ls
    where
        drop# :: Int# -> [a] -> [a]
        drop# 0# xs      = xs
        drop# _  xs@[]   = xs
        drop# m# (_:xs)  = drop# (m# -# 1#) xs

splitAt                :: Int -> [a] -> ([a],[a])
{-
splitAt (I# n#) ls@(x:xs)
  | n# <=# 0# = ([], ls)
  | otherwise = let (xs', xs'') = splitAt (I# (n# -# 1#)) xs in
                (x:xs', xs'')
-}
splitAt (I# n#) ls
  | isTrue# (n# <# 0#)    = ([], ls)
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
  | isTrue# (n0 <# 0#) = undef
  | otherwise = sub xs n0
     where
       sub :: [a] -> Int# -> a
       sub []     _ = undef
       sub (y:ys) n = if isTrue# (n ==# 0#)
                      then y
                      else sub ys (n -# 1#)

-- | Applied to a predicate and a list, 'any' determines if any element
-- of the list satisfies the predicate.  For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value for the predicate applied to an element at a finite index of a finite or infinite list.
any                     :: (a -> Bool) -> [a] -> Bool
any _ []        = False
any p (x:xs)    = p x || any p xs


-- | Applied to a predicate and a list, 'all' determines if all elements
-- of the list satisfy the predicate. For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value for the predicate applied to an element at a finite index of a finite or infinite list.
all                     :: (a -> Bool) -> [a] -> Bool
all _ []        =  True
all p (x:xs)    =  p x && all p xs

-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse                 :: [a] -> [a]
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

-- | 'elem' is the list membership predicate, usually written in infix form,
-- e.g., @x \`elem\` xs@.  For the result to be
-- 'False', the list must be finite; 'True', however, results from an element equal to @x@ found at a finite index of a finite or infinite list.
elem                    :: (Eq a) => a -> [a] -> Bool
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys


-- | 'notElem' is the negation of 'elem'.
notElem                 :: (Eq a) => a -> [a] -> Bool
notElem _ []    =  True
notElem x (y:ys)=  x /= y && notElem x ys

-- | Map a function over a list and concatenate the results.
concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = foldr (++) []

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the list:
-- 
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.

break                   :: (a -> Bool) -> [a] -> ([a],[a])
-- HBC version (stolen)
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)

-- | 'and' returns the conjunction of a Boolean list.  For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value at a finite index of a finite or infinite list.
and                     :: [Bool] -> Bool
and []          =  True
and (x:xs)      =  x && and xs

-- | 'or' returns the disjunction of a Boolean list.  For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value at a finite index of a finite or infinite list.
or                      :: [Bool] -> Bool
or []           =  False
or (x:xs)       =  x || or xs

-- | 'lookup' @key assocs@ looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- | 'zip' takes two lists and returns a list of corresponding pairs.
-- If one input list is short, excess elements of the longer list are
-- discarded.
zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []

-- | 'zip3' takes three lists and returns a list of triples, analogous to
-- 'zip'.
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'zipWith' (+)@ is applied to two lists to produce the
-- list of corresponding sums.
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []

-- | The 'zipWith3' function takes a function which combines three
-- elements, as well as three lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []

-- | 'unzip' transforms a list of pairs into a list of first components
-- and a list of second components.
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

-- | The 'unzip3' function takes a list of triples and returns three
-- lists, analogous to 'unzip'.
unzip3   :: [(a,b,c)] -> ([a],[b],[c])
{-# INLINE unzip3 #-}
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                  ([],[],[])

{-# NOINLINE errorEmptyList #-}
errorEmptyList :: String -> a
errorEmptyList fun = errorEmptyList fun
