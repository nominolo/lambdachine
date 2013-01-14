{-# LANGUAGE NoImplicitPrelude, Rank2Types, MagicHash #-}
module Text.ParserCombinators.ReadP
  ( 
  ReadP,      -- :: * -> *; instance Functor, Monad, MonadPlus

  get,        -- :: ReadP Char
  look,       -- :: ReadP String
  (+++),      -- :: ReadP a -> ReadP a -> ReadP a
  (<++),      -- :: ReadP a -> ReadP a -> ReadP a
  gather,     -- :: ReadP a -> ReadP (String, a)
  
  -- * Other operations
  pfail,      -- :: ReadP a
  eof,        -- :: ReadP ()
  satisfy,    -- :: (Char -> Bool) -> ReadP Char
  char,       -- :: Char -> ReadP Char
  string,     -- :: String -> ReadP String
  munch,      -- :: (Char -> Bool) -> ReadP String
  munch1,     -- :: (Char -> Bool) -> ReadP String
  skipSpaces, -- :: ReadP ()
  choice,     -- :: [ReadP a] -> ReadP a
  count,      -- :: Int -> ReadP a -> ReadP [a]
  between,    -- :: ReadP open -> ReadP close -> ReadP a -> ReadP a
  option,     -- :: a -> ReadP a -> ReadP a
  optional,   -- :: ReadP a -> ReadP ()
  many,       -- :: ReadP a -> ReadP [a]
  many1,      -- :: ReadP a -> ReadP [a]
  skipMany,   -- :: ReadP a -> ReadP ()
  skipMany1,  -- :: ReadP a -> ReadP ()
  sepBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  sepBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  chainr,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  chainr1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  manyTill,   -- :: ReadP a -> ReadP end -> ReadP [a]

  ReadS,
  readP_to_S,
  readS_to_P, -- :: ReadS a -> ReadP a
  )
where

import GHC.Base
import Control.Monad ( MonadPlus(..), sequence, liftM2 )
import Data.List ( replicate, null )
import Data.Char ( isSpace )

infixr 5 +++, <++

type ReadS a = String -> [(a,String)]

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final [(a,String)] -- invariant: list is non-empty!

instance Monad P where
  return x = Result x Fail

  (Get f)      >>= k = Get (\c -> f c >>= k)
  (Look f)     >>= k = Look (\s -> f s >>= k)
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

newtype ReadP a = R (forall b . (a -> P b) -> P b)

instance Functor ReadP where
  fmap h (R f) = R (\k -> f (k . h))

instance Monad ReadP where
  return x  = R (\k -> k x)
  fail _    = R (\_ -> Fail)
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance MonadPlus ReadP where
  mzero = pfail
  mplus = (+++)

final :: [(a,String)] -> P a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

run :: P a -> ReadS a
run (Get f)      (c:s) = run (f c) s
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Final r)    _     = r
run _            _     = []

get :: ReadP Char
get = R Get

look :: ReadP String
look = R Look

pfail :: ReadP a
pfail = R (\_ -> Fail)

(+++) :: ReadP a -> ReadP a -> ReadP a
R f1 +++ R f2 = R (\k -> f1 k `mplus` f2 k)

(<++) :: ReadP a -> ReadP a -> ReadP a
R f0 <++ q =
  do s <- look
     probe (f0 return) s 0#
 where
  probe (Get f)        (c:s) n = probe (f c) s (n+#1#)
  probe (Look f)       s     n = probe (f s) s n
  probe p@(Result _ _) _     n = discard n >> R (p >>=)
  probe (Final r)      _     _ = R (Final r >>=)
  probe _              _     _ = q

  discard 0# = return ()
  discard n  = get >> discard (n-#1#)

gather :: ReadP a -> ReadP (String, a)
gather (R m)
  = R (\k -> gath id (m (\a -> return (\s -> k (s,a)))))  
 where
  gath :: (String -> String) -> P (String -> P b) -> P b
  gath l (Get f)      = Get (\c -> gath (l.(c:)) (f c))
  gath _ Fail         = Fail
  gath l (Look f)     = Look (\s -> gath l (f s))
  gath l (Result k p) = k (l []) `mplus` gath l p
  gath _ (Final _)    = error "do not use readS_to_P in gather!"

-- ---------------------------------------------------------------------------

satisfy :: (Char -> Bool) -> ReadP Char
satisfy p = do c <- get; if p c then return c else pfail

char :: Char -> ReadP Char
char c = satisfy (c ==)

eof :: ReadP ()
eof = do { s <- look 
         ; if null s then return () 
                     else pfail }

string :: String -> ReadP String
string this = do s <- look; scan this s
 where
  scan []     _               = do return this
  scan (x:xs) (y:ys) | x == y = do _ <- get; scan xs ys
  scan _      _               = do pfail

munch :: (Char -> Bool) -> ReadP String
munch p =
  do s <- look
     scan s
 where
  scan (c:cs) | p c = do _ <- get; s <- scan cs; return (c:s)
  scan _            = do return ""

munch1 :: (Char -> Bool) -> ReadP String
munch1 p =
  do c <- get
     if p c then do s <- munch p; return (c:s)
            else pfail

choice :: [ReadP a] -> ReadP a
choice []     = pfail
choice [p]    = p
choice (p:ps) = p +++ choice ps

skipSpaces :: ReadP ()
skipSpaces =
  do s <- look
     skip s
 where
  skip (c:s) | isSpace c = do _ <- get; skip s
  skip _                 = do return ()

count :: Int -> ReadP a -> ReadP [a]
count n p = sequence (replicate n p)

between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
between open close p = do _ <- open
                          x <- p
                          _ <- close
                          return x

option :: a -> ReadP a -> ReadP a
option x p = p +++ return x

optional :: ReadP a -> ReadP ()
optional p = (p >> return ()) +++ return ()

many :: ReadP a -> ReadP [a]
many p = return [] +++ many1 p

many1 :: ReadP a -> ReadP [a]
many1 p = liftM2 (:) p (many p)

skipMany :: ReadP a -> ReadP ()
skipMany p = many p >> return ()

skipMany1 :: ReadP a -> ReadP ()
skipMany1 p = p >> skipMany p

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: ReadP a -> ReadP sep -> ReadP [a]
endBy p sep = many (do x <- p ; _ <- sep ; return x)

endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
endBy1 p sep = many1 (do x <- p ; _ <- sep ; return x)

chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainr p op x = chainr1 p op +++ return x

chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainl p op x = chainl1 p op +++ return x

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 +++ return x

chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 +++ return x

manyTill :: ReadP a -> ReadP end -> ReadP [a]
manyTill p end = scan
  where scan = (end >> return []) <++ (liftM2 (:) p scan)


readP_to_S :: ReadP a -> ReadS a
readP_to_S (R f) = run (f return)


readS_to_P :: ReadS a -> ReadP a
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a,s') <- r s, bs'' <- run (k a) s']))
