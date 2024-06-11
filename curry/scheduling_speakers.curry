{- 
  
  Scheduling speakers in Curry CLP.FD

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  This is a port of my Picat model http://hakank.org/picat/scheduling_speakers.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtilsCLPFD (elementVal)
import CLP.FD
import Debug.Trace


available :: [[Int]]
available =  [
         [2,3,4,5],    -- 2) the only one with 5 after speaker F -> 0
         [2,3],        -- 5) 2 or 3
         [1,2,3,4],    -- 3) only with 4 after F -> 0 and A -> 5
         [1,2,3],      -- 4) only with 1 after C -> 4 and F -> 0 
         [2,3],        -- 5) 2 or 3
         [0,1,2,3,4,5] -- 1) the only with 0
       ]

--
-- ensure that x[i] is any of available[i]
-- (elementVal requires a decision variable as first parameter, here i)
--

-- using non determinism
check :: [[Int]] -> [CLP.FD.FDExpr] -> CLP.FD.FDConstr
check []        _     = true
check (av:aas) (x:xs) = (fd (anyOf av) =# x) /\ check aas xs


scheduling_speakers :: [[Int]]
scheduling_speakers = let
                        n = 6
                        xs = take n (domain 0 (n-1))
                      in
                        solveFDAll [FirstFail,Bisect] xs $
                        allDifferent xs /\

                        -- Here are some variant of the same idea:
                        -- ensure that xs[i] is in available[i]

                        -- using elementVal
                        -- foldl1 (/\) [ elementVal i (map fd aa) x
                        --               | (x,aa) <-zip xs available,
                        --               -- elementVal requires a decision variable as first parameter 
                        --               let i = head (domain 0 ((length aa)-1)) 
                        --               ]

                        -- check available xs  -- using explicit non determinism

                        -- same idea as check but as a list comprehension
                        foldl1 (/\) [ fd (anyOf av) =# x | (x,av) <- zip xs available]

{-

  [5,2,4,1,3,0]
  [5,3,4,1,2,0]
  Execution time: 29 msec. / elapsed: 36 msec.

-}
main :: IO ()
main = mapM_ print $ scheduling_speakers