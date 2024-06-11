{- 
  
  Element test in Curry
  
  Test of (my) element constraint in CLP.FD.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD
import HakankUtilsCLPFD (elementVal)


elementTest1 =  let
                   n = 3
                   maxVal = 4
                   x = take n (domain 0 maxVal)
                   y = head (domain 0 (n-1))
                   z = head (domain 0 maxVal)
                in
                   -- solveFDAll [] (x ++ [y,z]) $
                   solveFDAll [] ([y,z] ++ x) $
                   (x !! 0) ># 2 /\
                   z <# 3 /\
                   Data.List.sum x =# 5 /\
                   elementVal y x z  -- z = x[y]


main = mapM_ print $ [ (x,y,z) | yzx <- elementTest1, let (y:z:x) = yzx]
