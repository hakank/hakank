{- 
  
  Test of the abs2 constraint in Curry CLP.FD

  CLP.FD does not support abs on decision variables, so I testing a variant
  using non determinism.

  And it seems to work...

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD

--
-- abs2 x y d
--  d = abs (x-y)
-- Note: It uses non determinism instead of a proper constraint decomposition.
--
abs2 :: CLP.FD.FDExpr -> CLP.FD.FDExpr -> CLP.FD.FDExpr ->  CLP.FD.FDConstr
abs2 x y d = x =# y /\ d =# (fd 0)
abs2 x y d = x >=# y /\ d =# (x - y)
abs2 x y d = x <# y /\ d =# (y - x)

test1 :: [[Int]]
test1 = let
          [x,y,d] = take 3 (domain 0 10)
        in
          solveFDAll [] [x,y,d] $
          abs2 x y d

main :: IO ()
main = mapM_ print test1

         