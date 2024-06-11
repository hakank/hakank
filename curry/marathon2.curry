{- 
  
  Marathon puzzle in Curry CLP.FD

  From Xpress example
  http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
  """
  Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
  have arrived as the first six at the Paris marathon.
  Reconstruct their arrival order from the following
  information:
  a) Olivier has not arrived last
  b) Dominique, Pascal and Ignace have arrived before Naren
     and Olivier
  c) Dominique who was third last year has improved this year.
  d) Philippe is among the first four.
  e) Ignace has arrived neither in second nor third position.
  f) Pascal has beaten Naren by three positions.
  g) Neither Ignace nor Dominique are on the fourth position.
  
     (c) 2002 Dash Associates
    author: S. Heipcke, Mar. 2002
  """

  This is a port of my Picat model http://hakank.org/picat/marathon2.pi


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD



marathon2 = let
               n = 6
               runners = take 6 (domain 1 6)
               [dominique, ignace, naren, olivier, philippe, pascal] = runners
            in
               solveFD [] runners $
               allDifferent runners /\
               
               -- a: Olivier not last
               olivier /=# n /\

               -- b: Dominique, Pascal and Ignace before Naren and Olivier
               dominique  <# naren /\
               dominique  <# olivier /\
               pascal     <# naren /\
               pascal     <# olivier /\
               ignace     <# naren /\
               ignace     <# olivier /\
   
               -- c: Dominique better than third
               dominique  <# 3 /\ 
   
               -- d: Philippe is among the first four
               philippe   <=# 4 /\
   
               -- e: Ignace neither second nor third
               ignace     /=# 2 /\ 
               ignace     /=# 3 /\ 
   
               -- f: Pascal three places earlier than naren
               pascal + 3 =# naren /\ 
   
               -- g: neither Ignace nor Dominique on fourth position
               ignace     /=# 4 /\
               dominique  /=# 4

{-

  [2,1,6,5,4,3]
  Execution time: 22 msec. / elapsed: 27 msec.

-}
main = marathon2

{-
   Order of arrivals

  ("Ignace",1)
  ("Dominique",2)
  ("Pascal",3)
  ("Philippe",4)
  ("Olivier",5)
  ("Naren",6)
  Execution time: 49 msec. / elapsed: 48 msec.

-}
main2 =   
         mapM_ print $ sortBy (\a b -> snd a < snd b) $ zip ["Dominique", "Ignace", "Naren", "Olivier", "Philippe", "Pascal"] marathon2