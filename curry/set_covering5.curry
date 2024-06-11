{- 
  
  Set covering, scheduling in Curry CLD.FD

  Example from the Swedish book 
  Lundgren, Ronnqvist, Varbrand "Optimeringslara", 
  ("Optimization theory"), page 410.
  
  Work schedule for a security company with three employees: Anders, Lotta, Per
  (common Swedish names).
 
  The tasks has different qualifications, and each person
  may give preferences to each task (each person has 10 points to spread).
 
  Task     Time (h)           Anders           Lotta            Per
                         Qualify, Points  Qualify, Points  Qualify, Points
   1             4         X        5      X         4       
   2             3         X        2                        X       2
   3             3         X        1      X         2       X       4
   4             3                         X         2       X       1
   5             3         X        1                        X       1
   6             2         X        1                        X       2
   7             2                         X         1
   8             2                         X         1
 
  Each person must work 7 or 8 hour per night.
 
  From the table above, 19 different combinations is construed, given at
  page 412. See the model below for the combinations.
 
  The problem is now to assign one combination to each person so
  that all tasks is covered exactly once, and maximizing the total
  preference points.
 
  The answer from the book, page 413: 
  The combination is 1 (Anders), 13 (Lotta), 18 (Per) with the following
  tasks:
    Anders: 1,2    Lotta: 4,7,8    Per: 3,5,6
 
  The total preference points is 18.

  
  Note: There is another solution with preference points 18:
  tasks 3, 13, 14
    Anders: 1,5  Lotta: 4,7,8   Per: 2,3,6


  This is a port of my Picat model http://hakank.org/picat/set_covering5.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD



persons = ["Anders","Lotta","Per"],

-- which combination belongs to which person
-- 1: Anders, 2: Lotta, 3: Per
combinationsPerson = [             
                       -- Anders, comb. 1..6
                       1, 1, 1, 1, 1, 1,
             
                       -- Lotta, comb. 7..13
                       2, 2, 2, 2, 2, 2, 2,
             
                       -- Per, comb., 14..19
                       3, 3, 3, 3, 3, 3
              ]

 -- preference point per tasks
references = [ -- 1 2 3 4 5 6 7 8
                 [5,2,1,0,1,1,0,0], -- Anders
                 [4,0,2,2,0,0,1,1], -- Lotta
                 [0,2,4,1,1,2,0,0] -- Per
             ]

-- the combinations and the tasks they contain (set representation)
comb = [
         -- Anders
         [1,2],   -- 1
         [1,3],   -- 2
         [1,5],   -- 3
         [2,3,6], -- 4
         [2,5,6], -- 5
         [3,5,6], -- 6 
      
         -- Lotta
         [1,3],   -- 7
         [1,4],   -- 8
         [1,7,8], -- 9
         [3,4,7], -- 10
         [3,4,8], -- 11
         [3,7,8], -- 12
         [4,7,8], -- 13
      
         -- Per
         [2,3,6], -- 14
         [2,4,6], -- 15
         [2,5,6], -- 16
         [3,4,6], -- 17
         [3,5,6], -- 18 
         [4,5,6]  -- 19
       ]


set_covering5 = let
                   
                  xs = take numCombination (domain 0 1)
                  preferencePoints = take numPersons (domain 1 10)
                  z = head (domain 0 100) -- sum preference points
                in
                  solveFDAll [] (z:xs) $
                  z =# Data.List.sum preferencePoints
                  