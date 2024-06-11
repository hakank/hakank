{- 
  
  Horse Man problem in Curry

  From an example (horseman.alf) in ALF functional logic programming language:
  """
  M men and H horses have Heads heads Feet feet.
  How many men and horses have 8 heads and 20 feet?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
import HakankUtils

horseman men horses = (men+horses,2*men+4*horses)

-- Generate problem 0..20 men and 0..20 horses
genProblem = (men+horses, 2*men+4*horses) where
                men = makeChoice [0..20]
                horses = makeChoice [0..20]

genProblem2 maxMen maxHorses = (men+horses, 2*men+4*horses) where
                men = makeChoice [0..maxMen]
                horses = makeChoice [0..maxHorses]


-- Solve a problem:  How many mean and horses have heads heads and feet feet?
solveProblem (heads,feet) = x =:= makeChoice [0..20] &> y =:= makeChoice [0..20] &> horseman x y =:= (heads, feet) &> (x,y) where x,y free

-- [6,2]
-- i.e. 6 men and 2 horses have 8 heads and 20 feet
main = x =:= makeChoice [0..20] &> y =:= makeChoice [0..20] &> horseman x y =:= (8, 20) &> [x,y] where x,y free

main2 = solveProblem (8,20)

-- generate and solve all problems for 0..20 men and 0..20 horses
main3 = solveProblem $ genProblem

main4' = (heads,feet) =:= genProblem &> (men,horses) =:= solveProblem (heads,feet) &> (heads,feet,men,horses) where heads,feet,men,horses free

main4 = allValues $ main4'
