{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Lambdachine.Utils.Pretty
-- Copyright   : (c) Thomas Schilling 2009
-- License     : BSD-style
--
-- Maintainer  : nominolo@googlemail.com
-- Stability   : experimental
-- Portability : portable
--
module Lambdachine.Utils.Pretty
  ( module Lambdachine.Utils.Pretty
  )
where

import qualified Text.PrettyPrint.ANSI.Leijen as P

import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Vector as V
import Data.Monoid

import Debug.Trace

------------------------------------------------------------------------
-- * The @Pretty@ Class

class Pretty a where
  ppr :: a -> PDoc

{-
instance Monoid P.Doc where
  mempty = P.empty
  mappend = (P.<>)
-}

type PDoc = PrettyStyle -> P.Doc

instance Show PDoc where show = render
instance Eq PDoc where x == y = show x == show y

data PrettyStyle
  = DebugStyle
  | UserStyle

pretty :: Pretty a => a -> String
pretty x = P.displayS (P.renderPretty 0.8 100 (ppr x UserStyle)) ""
--pretty x = show (ppr x UserStyle)a

pprint :: Pretty a => a -> IO ()
pprint x = B.putStrLn $ B.fromString $ pretty x --render (ppr x)

debugPrint :: Pretty a => a -> IO ()
debugPrint x = B.putStrLn $ B.fromString $ show $ ppr x DebugStyle

render :: PDoc -> String
render d = show (d UserStyle)

debugRender :: PDoc -> String
debugRender d = show (d DebugStyle)

------------------------------------------------------------------------
-- * Combinators

-- ** Primitives

empty :: PDoc
empty _ = P.empty

char :: Char -> PDoc
char c _ = P.char c

text :: String -> PDoc
text s _ = P.text s

int :: Int -> PDoc
int i _ = P.int i

infixr 6 <> 
infixr 6 <+>
infixr 5 $$, $+$, <//>, </>

(<>) :: PDoc -> PDoc -> PDoc
(<>) d1 d2 sty = d1 sty P.<> d2 sty

(<+>) :: PDoc -> PDoc -> PDoc
(<+>) d1 d2 sty = d1 sty P.<+> d2 sty

($$) :: PDoc -> PDoc -> PDoc
($$) d1 d2 sty = d1 sty P.<$> d2 sty

($+$) :: PDoc -> PDoc -> PDoc
($+$) d1 d2 sty = d1 sty P.<$$> d2 sty

(<//>) :: PDoc -> PDoc -> PDoc
(<//>) d1 d2 sty = d1 sty P.<//> d2 sty

(</>) :: PDoc -> PDoc -> PDoc
(</>) d1 d2 sty = d1 sty P.</> d2 sty

linebreak :: PDoc
linebreak _ = P.linebreak

hcat :: [PDoc] -> PDoc
hcat ds sty = P.hcat (map ($ sty) ds)

hsep   :: [PDoc] -> PDoc
hsep ds sty = P.hsep (map ($ sty) ds)

vcat   :: [PDoc] -> PDoc
vcat ds sty = P.vcat (map ($ sty) ds)

cat    :: [PDoc] -> PDoc
cat ds sty = P.cat (map ($ sty) ds)

sep    :: [PDoc] -> PDoc
sep ds sty = P.sep (map ($ sty) ds)

fillCat   :: [PDoc] -> PDoc
fillCat ds sty = P.fillCat (map ($ sty) ds)

fillSep   :: [PDoc] -> PDoc
fillSep ds sty = P.fillSep (map ($ sty) ds)

nest   :: Int -> PDoc -> PDoc
nest n d sty = P.nest n (d sty)

align :: PDoc -> PDoc
align d sty = P.align (d sty)
-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Int -> PDoc -> PDoc
hang n d sty = P.hang n (d sty)

indent :: Int -> PDoc -> PDoc
indent n d sty = P.indent n (d sty)
fillBreak :: Int -> PDoc -> PDoc
fillBreak n d sty = P.fillBreak n (d sty)

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: PDoc -> [PDoc] -> [PDoc]
punctuate _ [] = []
punctuate p (d:ds) = go d ds
  where go d' []     = [d']
        go d' (e:es) = (d' <> p) : go e es

-- ** Parenthesis

parens :: PDoc -> PDoc
parens d sty = P.parens (d sty)

braces :: PDoc -> PDoc
braces d sty = P.braces (d sty)

brackets :: PDoc -> PDoc
brackets d sty = P.brackets (d sty)

angleBrackets :: PDoc -> PDoc
angleBrackets d = char '<' <> d <> char '>'

-- ** Symbols

comma :: PDoc
comma _ = P.comma

arrow :: PDoc
arrow _ = P.text "->"

colon :: PDoc
colon _ = P.colon

semi :: PDoc
semi _ = P.semi

-- | A string where words are automatically wrapped.
wrappingText :: String -> PDoc
wrappingText msg = fillSep $ map text $ words msg

textWords :: String -> [PDoc]
textWords msg = map text (words msg)

------------------------------------------------------------------------

-- ** Terminal Styles

withStyle :: (P.Doc -> P.Doc) -> PDoc -> PDoc
withStyle f d s = f (d s)

-- ansiTermStyle :: String -> PDoc -> PDoc
-- ansiTermStyle ansi d sty =
--   P.zeroWidthText ("\027[" ++ ansi ++ "m") P.<>
--   d sty P.<>
--   P.zeroWidthText "\027[0m"

-- ansiTermStyle2 :: String -> String -> PDoc -> PDoc
-- ansiTermStyle2 start end d sty =
--   P.zeroWidthText ("\027[" ++ start ++ "m") P.<>
--   d sty P.<>
--   P.zeroWidthText ("\027[" ++ end ++ "m")

bold :: PDoc -> PDoc
bold d s = P.bold (d s)

underline :: PDoc -> PDoc
underline = withStyle P.underline

keyword :: String -> PDoc
keyword = bold . text

colour1 :: PDoc -> PDoc
colour1 = withStyle P.cyan

colour2 :: PDoc -> PDoc
colour2 = withStyle P.red

pale :: PDoc -> PDoc
pale = withStyle P.dullwhite

varcolour :: PDoc -> PDoc
varcolour = id -- withStyle P.magenta

gblcolour :: PDoc -> PDoc
gblcolour = withStyle P.dullgreen

dconcolour :: PDoc -> PDoc
dconcolour = withStyle P.blue

-- ** Style-specific Combinators

ifDebugStyle :: PDoc -> PDoc
ifDebugStyle d sty@DebugStyle = P.dullwhite (d sty)
ifDebugStyle _d _ = P.empty

withDebugStyle :: PDoc -> PDoc
withDebugStyle d _ = d DebugStyle

-- ** Utils

commaSep :: [PDoc] -> [PDoc]
commaSep = punctuate comma

ppFill :: Int -> Int -> PDoc
ppFill digits val
  | digits <= 1 || val >= 10 ^ (digits - 1)
  = ppr val
  | otherwise
  = char '0' <> ppFill (digits - 1) val

------------------------------------------------------------------------
-- * Prelude Type Instances
instance Pretty PDoc where ppr = id
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  ppr (Left a) = ppr a
  ppr (Right b) = ppr b

instance Pretty Bool where
  ppr True = text "true"
  ppr False = text "false"

instance Pretty Int where
  ppr n = text (show n)

instance Pretty Integer where
  ppr n = text (show n)

instance Pretty a => Pretty (Maybe a) where
  ppr Nothing = text "(nothing)"
  ppr (Just a) = ppr a

instance (Pretty a, Pretty b) => Pretty (a,b) where
  ppr (a,b) = parens (sep [ppr a <> comma, ppr b])

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  ppr (a,b,c) = parens (sep [ppr a <> comma, ppr b <> comma, ppr c])
instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a,b,c,d) where
  ppr (a,b,c,d) =
    parens (sep [ppr a <> comma, ppr b <> comma, ppr c <> comma, ppr d])

instance Pretty s => Pretty (Set s) where
  ppr s = braces (fillSep (punctuate comma (map ppr (S.toList s))))

instance Pretty IS.IntSet where
  ppr s = braces (fillSep (punctuate comma (map ppr (IS.toList s))))

instance Pretty a => Pretty (V.Vector a) where
  ppr vec = ppr (V.toList vec)

instance Pretty a => Pretty [a] where
  ppr l = brackets (fillSep (punctuate comma (map ppr l)))

instance (Pretty k, Pretty a) => Pretty (Map k a) where
  ppr s = braces (vcat (punctuate comma (map ppr_elem (M.toList s))))
    where ppr_elem (k, v) = colour1 (ppr k) <> colon <+> ppr v

instance (Pretty a) => Pretty (IM.IntMap a) where
  ppr s = braces (fillSep (punctuate comma (map ppr_elem (IM.toList s))))
    where ppr_elem (k, v) = ppr k <> colon <+> ppr v
