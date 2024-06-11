-- Testing a single program for KICS2.

import Data.List

euler1a = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]