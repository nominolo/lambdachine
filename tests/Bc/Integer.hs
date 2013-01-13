{-# LANGUAGE NoImplicitPrelude #-}
module Bc.Integer where

import GHC.Base
import GHC.Num
import GHC.Real
import Data.List ( length )
import GHC.Show

test = (5 == (3 + 2 :: Integer)) &&
       (43289749238174983 * 4321890423092180 == 187093552650530454110662778932944 - 4) &&
       (-43289749238174983 * 4321890423092181 < 187093552650530454110662778932940) &&
       ((5 :: Integer) == fromIntegral (5 :: Int)) &&
       (length (show (43289749238174983 * 4321890423092180)) == 33)
