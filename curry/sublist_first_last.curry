{-
  From Curry: A Tutorial Introduction, page 18
  """
  Exercise 3 Define a function that takes a list a of integers and computes a 
  sublist l of a such that the last element of l is twice the first element. 
  E.g., given the list [3, 6, 2, 1, 4, 5] the sublists satisfying the required 
  constraint are [3, 6] and [2, 1, 4].
  """

-}

{-
  Intended solution:

-- Compute a sublist l of the integer argument list with the constraint
-- that the last element of l is twice the first.
-- E.g., the expected results of the test data are [3,6] and [2,1,4]

consublist :: [Int] -> [Int]
consublist (_++[x]++y++[z]++_) | 2*x == z = [x]++y++[z]

testdata :: [Int]
testdata = [3,6,2,1,4,5]

main :: [Int]
main = consublist testdata

-}

import Data.List
import HakankUtils

s xs = (_++x++_ =:= xs & (head x)*2=:= last x) &> x where x free


main = s [3,6,2,1,4,5]

-- Also generate the main list
main2 = x=:=makeVars 3 [1..29] &> y=:=s x &> [x,y] where x,y free