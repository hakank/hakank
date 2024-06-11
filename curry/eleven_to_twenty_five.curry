{- 
  
  From 11 to 25  in Curry

  The Bonus problem from 
  "Can you solve the giant cat army riddle? - Dan Finkel"
  https://www.youtube.com/watch?v=YeMVoJKn1Tg&feature=youtu.be
  @4:35
  """
  Get from 11 to 25 
  Operations available: 
    - x*2 
    - x-3
  """

  This is the shortest and unique solutions (length 8),
  [11,8,5,10,7,14,28,25]


  If we know the length of the sequence, it's easy
  > x =:= (take 8 $ iterate op 11) & last x =:= 25 where x free
  {x=[11,8,5,10,7,14,28,25]} True


  Cf http://hakank.org/picat/11_to_25.pi

  


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
    
-}

import Data.List
import Control.AllValues
import HakankUtils


op x = x*2
op x = x-3

-- The Giant Cat Army Puzzle (harder than 11->25)
-- See giant_cat_army_riddle.curry
-- op2 :: Int -> Int
op2 x = x+5
op2 x = x+7
-- sqrt(x)
op2 x = y =:= anyOf [1..x] &> y*y =:= x &> y where y free

-- This works, though it requires a max value for n
sseq start end max = (n=:=makeChoice [2..max] & x =:= (take n $ iterate op start) & last x =:= end) &> (n,x) where x,n free

-- General version
-- This does not yield any solution
sseq2 op1 op2 start end max = (n=:=makeChoice [2..max] &> x =:= (take n $ iterate (op1?op2) start) &> last x =:= end) &> (n,x) where x,n free

-- Let's assume that the solution is between 2..10 steps
main = (n=:=makeChoice [2..10] & x =:= (take n $ iterate op 11) & last x =:= 25) &> (n,x) where x,n free

main2 = sseq 11 25 10

main2b = head $ allValues $ sseq 11 25 10
main2c = getOneValue $ sseq 11 25 100 -- fast then using allValues

-- This does not work... No value found
main3 = sseq2 (\x -> x * 2) (\x -> x - 3) 11 25 10

-- out of memory
main4 = (n=:=makeChoice [2..25] & x =:= (take n $ iterate op2 0) & last x =:= 14) &> x where x,n free
