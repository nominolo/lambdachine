-- | Generate (X)HTML as debug output.
module Lambdachine.Interp.Html 
  ( module Lambdachine.Interp.Html
  , H.HTML(..)
  , H.showHtml, H.renderHtml, H.prettyHtml
  ) 
where

import Lambdachine.Interp.Types

import Text.XHtml (HTML(..), Html, (<<), (+++), (!))
import qualified Text.XHtml as H
import Data.Monoid


-- -------------------------------------------------------------------

defaultWrapper :: Html -> Html
defaultWrapper h =
  H.header << H.style << default_style 
    +++
  H.body << h
 where
   default_style = unlines $
     [ ".var{ color: #00f }",
       ".const { color: #090 }",
       ".hpvar { color: #c00 }"
     ]

--infixl 6 <>

--traceToHtml :: RecordState -> IO H.Html
--traceToHtml 

-- class ToHtml a where
--   toHtml :: a -> H.Html

insName :: String -> Html
insName nm = H.bold << nm

aVar :: HTML a => String -> a -> Html
aVar cl a = H.thespan ! [H.theclass ("var " ++ cl)] << a

instance HTML TRef where
  toHtml TNil = aVar "" '-'
  toHtml (TVar n) = aVar "var" n
  toHtml (TCst n c) = aVar "const" $ 'C' +++ n
  toHtml (THp n) = aVar "hpvar" n

instance HTML Int where
  toHtml n = toHtml (show n)

instance HTML IRIns where
  toHtml ins = case ins of
    Nop -> insName "nop"
    Loop -> insName "loop"
    SLoad n -> insName "ld_slot" +++ ' ' +++ n
    FLoad r o -> insName "ld_field" +++ ' ' +++ r +++ ", " +++ o
    LoadBase n -> insName "ld_base" +++ ' ' +++ n
    SetBase n -> insName "set_base" +++ ' ' +++ n
    
    