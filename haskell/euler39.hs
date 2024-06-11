{- 
  
  Euler #39 in Haskell

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils (collect,collect2)

euler39a :: (Int,Int)
euler39a = maximum $ map (\(n,c) -> (c,n)) $ collect $ sort [t | a <- [1..500], b <- [a..500], c <- [b..500], a^2 + b^2 == c^2, t <- [a+b+c], t <= 1000]


euler39b :: (Int,Int)
euler39b =  maximum $ map (\(n,c) -> (c,n)) $ collect $ sort [t | a <- squares, b <- squares, a <= b, elem (a + b) squares, t <- [sq a b], t <= 1000]
         where squares = [n^2 | n <- [1..500]]
               sq a b = floor $ (sqrt a2) + (sqrt b2) + (sqrt (a2+b2))
                    where a2 = fromIntegral a
                          b2 = fromIntegral b

-- Using collect2 instead of collect (skipping the map)
euler39c :: (Int,Int)
euler39c =  maximum $ collect2 $ sort [t | a <- squares, b <- squares, a < b, elem (a + b) squares, t <- [sq a b], t <= 1000]
         where squares = [n^2 | n <- [1..500]]
               sq a b = floor $ (sqrt a2) + (sqrt b2) + (sqrt (a2+b2))
                    where a2 = fromIntegral a
                          b2 = fromIntegral b



                               
main :: IO ()
main = do
         -- print euler39a -- (23.89 secs, 33,393,645,208 bytes)

         -- print euler39b -- (0.86 secs, 27,280,656 bytes)

         print euler39c -- (0.82 secs, 27,216,752 bytes)