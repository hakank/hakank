{- 
  
  Grocery problem in Curry

  From the Gecode example
  """
  A kid goes into a grocery store and buys four items. The cashier
  charges $7.11, the kid pays and is about to leave when the cashier
  calls the kid back, and says "Hold on, I multiplied the four items
  instead of adding them; I'll try again; Hah, with adding them the
  price still comes to $7.11". What were the prices of the four items?

  The model is taken from: Christian Schulte, Gert Smolka, Finite Domain
  Constraint Programming in Oz. A Tutorial. 2001.
  Available from: http://www.mozart-oz.org/documentation/fdt/
  """

  This is a port of my Picat model http://hakank.org/picat/grocery.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


grocery = let
             [a,b,c,d] = take 4 (domain 0 711)
          in
             solveFDAll [] [a,b,c,d] $
             a + b + c + d =# 711 /\ 
             a * b * c * d =# 711*100*100*100 /\
             a <=# b /\ b <=# c /\ c <=# d

{-

  [[120,125,150,316]]
  Execution time: 144 msec. / elapsed: 144 msec.

-}
main = grocery

