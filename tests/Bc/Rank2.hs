{-# LANGUAGE NoImplicitPrelude, Rank2Types #-}

module Bc.Rank2 where

import GHC.Base
import Control.Monad ( MonadPlus(..) )

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Final [(a, String)]
  | Result a (P a)

type ReadS a = String -> [(a,String)]

newtype ReadP a = R (forall b. (a -> P b) -> P b)

instance Monad P where
  return x = Result x Fail

  (Get f)      >>= k = Get (\c -> f c >>= k)
  -- (Look f)     >>= k = Look (\s -> f s >>= k)
  Fail         >>= _ = Fail
  (Result x p) >>= k = k x `mplus` (p >>= k)
  (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]

  fail _ = Fail

instance MonadPlus P where
  mzero = Fail

  -- most common case: two gets are combined
  Get f1     `mplus` Get f2     = Get (\c -> f1 c `mplus` f2 c)
  
  -- results are delivered as soon as possible
  Result x p `mplus` q          = Result x (p `mplus` q)
  p          `mplus` Result x q = Result x (p `mplus` q)

  -- fail disappears
  Fail       `mplus` p          = p
  p          `mplus` Fail       = p

  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r    `mplus` Final t    = Final (r ++ t)
  Final r    `mplus` Look f     = Look (\s -> Final (r ++ run (f s) s))
  Final r    `mplus` p          = Look (\s -> Final (r ++ run p s))
  Look f     `mplus` Final r    = Look (\s -> Final (run (f s) s ++ r))
  p          `mplus` Final r    = Look (\s -> Final (run p s ++ r))

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f     `mplus` Look g     = Look (\s -> f s `mplus` g s)
  Look f     `mplus` p          = Look (\s -> f s `mplus` p)
  p          `mplus` Look f     = Look (\s -> p `mplus` f s)

run :: P a -> String -> [(a, String)]
run (Get f)   (c:s)  = run (f c) s
run (Look f)     s   = run (f s) s
run (Final r) _      = r
run Fail      _      = []
run (Result x p) s   = (x,s) : run p s

final :: [(a,String)] -> P a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

readP_to_S :: ReadP a -> ReadS a
readP_to_S (R f) = run (f return)

p1 = Get (\c -> if c == 'a' then return (1 :: Int)
                            else Fail)
      `mplus`
     Get (\c -> if c == 'b' then return (2 :: Int)
                            else Fail)

test = run p1 "a" == [(1, "")]
