{- 
  
  Broken weights puzzle in Curry CLP.FD

  From
  http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrie’s 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  This is a port of my Picat model http://hakank.org/picat/broken_weights.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf)
import HakankUtilsCLPFD (increasingCLP)
import CLP.FD

brokenWeights :: Int -> Int -> [Int]
brokenWeights n m = let
                       weights = take n (domain 1 m)
                       x' = take (n*m) (domain (-1) 1)
                       x = chunksOf n x'
                    in
                       solveFD [] (weights ++ x') $
                       -- solveFD [] weights $                       
                       allDifferent weights /\
                       increasingCLP weights /\
                       (fd m) =# Data.List.sum weights /\
                       foldl1 (/\) [ scalarProduct weights xx Equ (fd j) |  (j,xx) <- zip [1..m] x ] 


{-

  weights: [1,3,9,27]
  (1,[1,0,0,0])
  (2,[-1,1,0,0])
  (3,[0,1,0,0])
  (4,[1,1,0,0])
  (5,[-1,-1,1,0])
  (6,[0,-1,1,0])
  (7,[1,-1,1,0])
  (8,[-1,0,1,0])
  (9,[0,0,1,0])
  (10,[1,0,1,0])
  (11,[-1,1,1,0])
  (12,[0,1,1,0])
  (13,[1,1,1,0])
  (14,[-1,-1,-1,1])
  (15,[0,-1,-1,1])
  (16,[1,-1,-1,1])
  (17,[-1,0,-1,1])
  (18,[0,0,-1,1])
  (19,[1,0,-1,1])
  (20,[-1,1,-1,1])
  (21,[0,1,-1,1])
  (22,[1,1,-1,1])
  (23,[-1,-1,0,1])
  (24,[0,-1,0,1])
  (25,[1,-1,0,1])
  (26,[-1,0,0,1])
  (27,[0,0,0,1])
  (28,[1,0,0,1])
  (29,[-1,1,0,1])
  (30,[0,1,0,1])
  (31,[1,1,0,1])
  (32,[-1,-1,1,1])
  (33,[0,-1,1,1])
  (34,[1,-1,1,1])
  (35,[-1,0,1,1])
  (36,[0,0,1,1])
  (37,[1,0,1,1])
  (38,[-1,1,1,1])
  (39,[0,1,1,1])
  (40,[1,1,1,1])
  Execution time: 159 msec. / elapsed: 158 msec.

-}
main :: IO ()
main = let
          n = 4
          m = 40
          (weights,xs) = splitAt n $ brokenWeights n m
        in
          sequence_ [putStrLn ("weights: " ++ (show weights)),
                     mapM_ print $ zip [1..40] ((chunksOf n) xs )]

