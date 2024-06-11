{- 
  
  XKCD subset problem in Curry

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total of exact 15.05.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


xkcd287' prices total = let
                          x = take (length prices) (domain 0 10)
                        in
                          solveFDAll [] x $
                          scalarProduct prices x Equ total


-- Convert float to integers (* 100)
xkcd287 = xkcd287' [215, 275, 335, 355, 420, 580]  1505


main = do
         -- PAKCS:
         -- [[1,0,0,2,0,1],[7,0,0,0,0,0]]
         -- Execution time: 28 msec. / elapsed: 32 msec.
         print xkcd287
