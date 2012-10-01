{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module Bc.SharedFail where

import GHC.Prim
import GHC.Types
import GHC.Num
import GHC.Base
-- import Prelude ( print )

rotate :: Int -> [a] -> [a]
rotate 2 (x1:x2:xs) = x2:x1:xs
rotate 3 (x1:x2:x3:xs) = x2:x3:x1:xs
rotate 4 (x1:x2:x3:x4:xs) = x2:x3:x4:x1:xs
rotate 5 (x1:x2:x3:x4:x5:xs) = x2:x3:x4:x5:x1:xs
rotate 6 (x1:x2:x3:x4:x5:x6:xs) = x2:x3:x4:x5:x6:x1:xs
rotate 7 (x1:x2:x3:x4:x5:x6:x7:xs) = x2:x3:x4:x5:x6:x7:x1:xs
rotate 8 (x1:x2:x3:x4:x5:x6:x7:x8:xs) = x2:x3:x4:x5:x6:x7:x8:x1:xs
rotate 9 (x1:x2:x3:x4:x5:x6:x7:x8:x9:xs) = x2:x3:x4:x5:x6:x7:x8:x9:x1:xs
rotate 10 (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:xs) = x2:x3:x4:x5:x6:x7:x8:x9:x10:x1:xs
rotate n (x:xs) = rotate' n xs
    where rotate' 1 xs     = x:xs
          rotate' n (x:xs) = x:rotate' (n-1) xs

-- test = rotate 2 [1::Int, 2, 3] == [2, 1, 3]

test = rotate 8 [1::Int, 2, 3, 4, 5, 6, 7, 8, 9] == [2,3,4,5,6,7,8,1,9]
