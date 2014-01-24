{-# LANGUAGE FlexibleInstances #-}
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
  , (<>)
  )
where

import qualified Text.PrettyPrint.ANSI.Leijen as P

import Control.Applicative
import Data.Functor.Identity
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Vector as V
import Data.Monoid
import DynFlags ( DynFlags )


import Debug.Trace

------------------------------------------------------------------------
-- * Global Environment Stuff

newtype GlobalEnv = GlobalEnv 
  { envDynFlags :: DynFlags }

class HasGlobalEnv env where
  -- | A lens for reading and writing the global environment.  Use
  -- 'mkGlobalEnvL' to construct a lens from a getter and a setter.
  --
  -- You may can use this with the utilities from the @lens@ package, but for
  -- convenience, there are also simple getter and setter utilities via
  -- 'viewGlobalEnv' and 'setGlobalEnv'.
  globalEnvL :: Functor f => (GlobalEnv -> f GlobalEnv) -> env -> f env

mkGlobalEnvL :: Functor f =>
                (env -> GlobalEnv) -> (env -> GlobalEnv -> env)
             -> (GlobalEnv -> f GlobalEnv) -> env -> f env
mkGlobalEnvL getE setE f env = setE env <$> f (getE env)

viewGlobalEnv :: HasGlobalEnv env => env -> GlobalEnv
viewGlobalEnv env = getConst $ globalEnvL Const env

setGlobalEnv :: HasGlobalEnv env => env -> GlobalEnv -> env
setGlobalEnv env newE = runIdentity $ globalEnvL (Identity . const newE) env

instance HasGlobalEnv PDocContext where
  globalEnvL = mkGlobalEnvL pdocGlobalEnv (\ctx ge -> ctx{ pdocGlobalEnv = ge })


------------------------------------------------------------------------
-- * The @Pretty@ Class

class Pretty a where
  ppr :: a -> PDoc

data PDocContext = PDocContext
  { pdocStyle     :: !PrettyStyle
  , pdocGlobalEnv :: !GlobalEnv
  }

data PrettyStyle
  = DebugStyle
  | UserStyle
  deriving (Eq, Ord, Show)

newtype PDoc = PDoc{ runPDoc :: PDocContext -> P.Doc }

instance Monoid PDoc where
  mempty = PDoc $ \_ -> P.empty
  mappend d1 d2 = PDoc $ \env -> runPDoc d1 env P.<> runPDoc d2 env

{-
instance Show PDoc where show = render
instance Eq PDoc where x == y = show x == show y
-}


pretty :: Pretty a => GlobalEnv -> a -> String
pretty env x = render env (ppr x)

pprint :: Pretty a => GlobalEnv -> a -> IO ()
pprint env x = B.putStrLn $ B.fromString $ pretty env x

debugPrint :: Pretty a => GlobalEnv -> a -> IO ()
debugPrint env x = B.putStrLn $ B.fromString $ pretty env (withDebugStyle (ppr x))

render :: GlobalEnv -> PDoc -> String
render env d = P.displayS (P.renderPretty 0.8 100 $ 
               runPDoc d $! PDocContext UserStyle env) ""

debugRender :: GlobalEnv -> PDoc -> String
debugRender env d = render env (withDebugStyle d)

------------------------------------------------------------------------
-- * Combinators

-- ** Primitives

liftP :: P.Doc -> PDoc
liftP doc = PDoc $ \_ -> doc

liftP1 :: (P.Doc -> P.Doc) -> PDoc -> PDoc
liftP1 f d1 = PDoc $ \env -> f (runPDoc d1 env)

liftP2 :: (P.Doc -> P.Doc -> P.Doc) -> PDoc -> PDoc -> PDoc
liftP2 f d1 d2 = PDoc $ \env -> runPDoc d1 env `f` runPDoc d2 env

liftPn :: ([P.Doc] -> P.Doc) -> [PDoc] -> PDoc
liftPn f ds = PDoc $ \env -> f [ runPDoc d env | d <- ds ]

empty :: PDoc
empty = liftP P.empty

char :: Char -> PDoc
char c = liftP $ P.char c

text :: String -> PDoc
text s = liftP $ P.text s

int :: Int -> PDoc
int i = liftP $ P.int i

-- infixr 6 <>   -- same as Data.Monoid.<>
infixr 6 <+>
infixr 5 $$, $+$, <//>, </>

-- Monoid instance for functions and P.Doc do the same
-- (<>) :: PDoc -> PDoc -> PDoc
-- (<>) d1 d2 sty = d1 sty P.<> d2 sty

(<+>) :: PDoc -> PDoc -> PDoc
(<+>) = liftP2 (P.<+>)

($$) :: PDoc -> PDoc -> PDoc
($$) = liftP2 (P.<$>)

($+$) :: PDoc -> PDoc -> PDoc
($+$) = liftP2 (P.<$$>)

(<//>) :: PDoc -> PDoc -> PDoc
(<//>) = liftP2 (P.<//>)

(</>) :: PDoc -> PDoc -> PDoc
(</>) = liftP2 (P.</>)

linebreak :: PDoc
linebreak = liftP $ P.linebreak

hcat :: [PDoc] -> PDoc
hcat = liftPn P.hcat

hsep   :: [PDoc] -> PDoc
hsep = liftPn P.hsep

vcat   :: [PDoc] -> PDoc
vcat = liftPn P.vcat

cat    :: [PDoc] -> PDoc
cat = liftPn P.cat

sep    :: [PDoc] -> PDoc
sep = liftPn P.sep

fillCat   :: [PDoc] -> PDoc
fillCat = liftPn P.fillCat

fillSep   :: [PDoc] -> PDoc
fillSep = liftPn P.fillSep

nest   :: Int -> PDoc -> PDoc
nest n = liftP1 $ P.nest n

align :: PDoc -> PDoc
align = liftP1 P.align

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Int -> PDoc -> PDoc
hang n = liftP1 $ P.hang n

indent :: Int -> PDoc -> PDoc
indent n = liftP1 $ P.indent n

fillBreak :: Int -> PDoc -> PDoc
fillBreak n = liftP1 $ P.fillBreak n

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: PDoc -> [PDoc] -> [PDoc]
punctuate _ [] = []
punctuate p (d:ds) = go d ds
  where go d' []     = [d']
        go d' (e:es) = (d' <> p) : go e es

-- ** Parenthesis

parens :: PDoc -> PDoc
parens = liftP1 P.parens

braces :: PDoc -> PDoc
braces = liftP1 P.braces

brackets :: PDoc -> PDoc
brackets = liftP1 P.brackets

angleBrackets :: PDoc -> PDoc
angleBrackets d = char '<' <> d <> char '>'

-- ** Symbols

comma :: PDoc
comma = liftP P.comma

arrow :: PDoc
arrow = text "->"

colon :: PDoc
colon = char ':'

semi :: PDoc
semi = char ';'

-- | A string where words are automatically wrapped.
wrappingText :: String -> PDoc
wrappingText msg = fillSep $ map text $ words msg

textWords :: String -> [PDoc]
textWords msg = map text (words msg)

------------------------------------------------------------------------

-- ** Terminal Styles

withStyle :: (P.Doc -> P.Doc) -> PDoc -> PDoc
withStyle = liftP1

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
bold = withStyle P.bold

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
ifDebugStyle d = PDoc $ \env ->
  case pdocStyle env of
    DebugStyle -> P.dullwhite (runPDoc d env)
    _          -> P.empty

withDebugStyle :: PDoc -> PDoc
withDebugStyle d = PDoc $ \env -> runPDoc d env{ pdocStyle = DebugStyle }

withGlobalEnv :: (GlobalEnv -> PDoc) -> PDoc
withGlobalEnv k = PDoc $ \env -> runPDoc (k (viewGlobalEnv env)) env

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
