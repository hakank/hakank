{- 
  
  A round of golf puzzle in Curry CLP.FD

  From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
  """
  Title: A Round of Golf
  Author: Ellen K. Rodehorst
  Publication: Dell Favorite Logic Problems
  Issue: Summer, 2000
  Puzzle #: 9
  Stars: 1
 
  When the Sunny Hills Country Club golf course isn't in use by club members, 
  of course, it's open to the club's employees. Recently, Jack and three other 
  workers at the golf course got together on their day off to play a round of 
  eighteen holes of golf. 
  Afterward, all four, including Mr. Green, went to the clubhouse to total 
  their scorecards. Each man works at a different job (one is a short-order 
  cook), and each shot a different score in the game. No one scored below 
  70 or above 85 strokes. From the clues below, can you discover each man's 
  full name, job and golf score?
  
  1. Bill, who is not the maintenance man, plays golf often and had the lowest 
  score of the foursome.
  2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten 
  strokes more than the pro-shop clerk.
  3. In some order, Frank and the caddy scored four and seven more strokes than 
  Mr. Sands.
  4. Mr. Carter thought his score of 78 was one of his better games, even 
     though Frank's score  was lower.
  5. None of the four scored exactly 81 strokes.
  
  Determine: First Name - Last Name - Job - Score 
  """

  Compare with the F1 model: 
  http://www.f1compiler.com/samples/A 20Round 20of 20Golf.f1.html

  Solution:
             Jack, Bill, Paul, Frank
             Clubb Sands Carter Green
             maint cook  caddy clerk
             85    71    78    75

  This is a port of my Picat model http://hakank.org/picat/a_round_of_golf.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf,inverse',inverseLookup,zip4)
import HakankUtilsCLPFD (elementVal)
import CLP.FD


--
-- Non-det version for the disjunction the two constraints regarding scores of
-- Frank, Sands and the Caddy.
-- (CLP.FD does not support disjunction of contraints)
--
constraint1 :: CLP.FD.FDExpr -> CLP.FD.FDExpr -> CLP.FD.FDExpr -> CLP.FD.FDConstr
constraint1 scorefrank scoresands scorecaddy = (scorefrank =# scoresands + 4) /\ (scorecaddy =# scoresands + 7)
constraint1 scorefrank scoresands scorecaddy = (scorefrank =# scoresands + 7) /\ (scorecaddy =# scoresands + 4)

a_round_of_golf :: [Int]
a_round_of_golf = let
                    n = 4
                    n1 = n-1

                    jack  = 0
                    bill  = 1
                    paul  = 2
                    frank = 3
                    -- firstname = [jack, bill, paul, frank]

                    lastName = take n (domain 0 n1)
                    [green,clubb,sands,carter] = lastName

                    job = take n (domain 0 n1)
                    [cook,maintenanceman,clerk,caddy] = job

                    score = take n (domain 70 85)
                    [scorejack,scorebill,scorepaul,scorefrank] = score

                    -- These are needed to support the elementVal constraint
                    [scoreclerk,scoreclubb,scoresands,scorecaddy,scorecarter] = take 5 (domain 70 85)

                  in
                    solveFD [] (concat [lastName,job,score]) $
                    allDifferent lastName /\
                    allDifferent job      /\
                    allDifferent score    /\

                    -- 1. Bill, who is not the maintenance man, plays golf often and had 
                    --    the lowest score of the foursome.
                    bill /=# maintenanceman /\

                    scorebill <# scorejack /\
                    scorebill <# scorepaul /\
                    scorebill <# scorefrank /\
   
                    -- 2. Mr. Clubb, who isn"t Paul, hit several balls into the woods and 
                    --    scored ten strokes more than the pro-shop clerk.
                    clubb /=# paul /\

                    elementVal clubb score scoreclubb  /\
                    elementVal clerk score scoreclerk /\
                    scoreclubb =# scoreclerk + 10 /\
       
                    -- 3. In some order, Frank and the caddy scored four and seven more 
                    --    strokes than Mr. Sands.
                    frank /=# caddy /\
                    frank /=# sands /\
                    caddy /=# sands /\
   
                    elementVal sands score scoresands /\
                    elementVal caddy score scorecaddy /\
                    elementVal carter score scorecarter /\

                    -- The following disjunction does not work since CLP.FD does not support
                    -- disjunctions of constraints (what I know), so we have to fix this using
                    -- non-determinism. But it works!
                    {-
                       ((scorefrank =# scoresands + 4 /\ scorecaddy =# scoresands + 7)
                         \/
                       (scorefrank =# scoresands + 7 /\ scorecaddy =# scoresands + 4)
                       ) /\
                    -}
                    constraint1 scorefrank scoresands scorecaddy /\
                   
                    -- 4. Mr. Carter thought his score of 78 was one of his better games, even 
                    -- though Frank"s score was lower.
                    frank /=# carter /\
   
                    scorecarter =# 78 /\
                    scorefrank <# scorecarter /\

                    -- 5. None of the four scored exactly 81 strokes.
                    -- foreach(S in Score) S /=# 81 end
                    foldl1 (/\) [s /=# 81 | s <- score]


{-

  [3,0,1,2,1,0,3,2,85,71,78,75]
  Execution time: 290 msec. / elapsed: 291 msec.

-}
main :: [Int]
main = a_round_of_golf

{-

  ("Jack","Clubb","Maintenance man",85)
  ("Bill","Sands","Cook",71)
  ("Paul","Carter","Caddy",78)
  ("Frank","Green","Clerk",75)
  Execution time: 318 msec. / elapsed: 318 msec.

-}
main2 :: IO ()
main2 = let 
         firstNameS = ["Jack", "Bill", "Paul", "Frank"]
         lastNameS  = ["Green","Clubb","Sands","Carter"] 
         jobS       = ["Cook","Maintenance man","Clerk","Caddy"]
         [lastName, job, score] = chunksOf 4 a_round_of_golf
       in 
         mapM_ print $ zip4 firstNameS (inverseLookup lastName lastNameS) (inverseLookup job jobS) score

