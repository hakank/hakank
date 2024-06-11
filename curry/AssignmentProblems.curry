{- 
  
  Assignments problems in Curry

  For use with assignments.curry and assignments2.curry


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

module AssignmentProblems where


--
-- Data
--

--
-- Data from 
-- Winston "Operations Research", Assignment Problems, page 393f
-- added the fifth column
-- See http://www.hakank.org/minizinc/assignment.mzn
--
cost1 :: Num a => ([Char], [[a]])
cost1 = ("minimize", [[14,  5, 8,  7, 15],
                      [ 2, 12, 6,  5,  3],
                      [ 7,  8, 3,  9,  7],
                      [ 2,  4, 6, 10,  1]])

-- 
-- Winston "Operations Research", page 398, swimming team example
-- (original version]
-- See http://www.hakank.org/minizinc/assignment2.mzn 
--
cost2 :: Num a => ([Char], [[a]])
cost2 = ("minimize", [[54, 54, 51, 53], 
                      [51, 57, 52, 52],
                      [50, 53, 54, 56],
                      [56, 54, 55, 53]])


-- 
-- Winston "Operations Research", page 398, swimming team example
-- See http://www.hakank.org/minizinc/assignment2_2.mzn 
-- expanded version
--
cost3 :: Num a => ([Char], [[a]])
cost3 = ("minimize",  [[54, 54, 51, 53,   50,60,70,80,90,100], 
                       [51, 57, 52, 52,   40,50,60,70,80, 90],
                       [50, 53, 54, 56,   40,50,60,80,93, 69],
                       [56, 54, 55, 53,   60,80,40,60,50,100]])


--
-- Winston "Operations Research", page 399
-- 
-- """
-- Tom Cruise, Freddy Prinze Jr, Harrison Ford, and Matt LeBlanc
-- are marooned on a desert island with Jennifer Anniston,
-- Courtney Cos, Gwynneth Paltrow, and Julia Roberts.
-- The 'compatibility matrix' in Table 52 indicate how much happiness
-- each couple would experience if the spend all their time toghether.
-- The happiness earned by a couple is proportional to the fraction 
-- of time the spend toghether. 
-- ...
-- The optimal solution requires that that each person send all their
-- time with one person of the opposite sex, so this result is often
-- referred to as the Marriage Theorem.
-- """
--
-- See http://www.hakank.org/minizinc/assignment3.mzn

-- males:
-- 1 "Tom Cruise"
-- 2 "Freddie Prinz Jr"
-- 3 "Harrison Ford"
-- 4 "Mark LeBlanc"
--
-- females:
-- 1 "Jennifer Anniston"
-- 2 "Courtney Cox"
-- 3 "Gwynneth Paltrow"
-- 4 "Julia Roberts"
--
cost4 :: Num a => ([Char], [[a]])
cost4 = ("maximize",  [[7, 5, 8, 2],
                       [7, 8, 9, 4],
                       [3, 5, 7, 9],
                       [5, 5, 6, 7]])


--
-- From
--  "SAS OR 9.1 User's Guide Mathematical Programming"
-- """
-- Consider assigning five programmers to five programming jobs. Each
-- programmer prefers specific programming job over others. [...] 
-- Suppose you ask each programmer to rank the jobs according to preference
-- (using 1 for the most preferred job and 5 for the least preffered job].
-- PROC ASSIGN maximizes the total preference of the group by minimizing the
-- sum of the preferences. 
-- 
--    PROGRAMMER     JOB1 JOB2 JOB3 JOB4 JOB5
--    PROGRAMMER1    4    1    3    5    2
--              2    2    1    3    4    5
--              3    3    2    4    1    5
--              4    2    3    4    5    1
--              5    4    2    3    1    5
-- 
-- """
-- 
-- See http://www.hakank.org/minizinc/assignment5.mzn
--
cost5 :: Num a => ([Char], [[a]])
cost5 = ("minimize", [[4, 1, 3, 5, 2],
                      [2, 1, 3, 4, 5],
                      [3, 2, 4, 1, 5],
                      [2, 3, 4, 5, 1],
                      [4, 2, 3, 1, 5]])



--
-- From GLPK:s example assign.mod:
-- """
-- The assignment problem is one of the fundamental combinatorial
-- optimization problems.
--
-- In its most general form, the problem is as follows:
--
-- There are a number of agents and a number of tasks. Any agent can be
-- assigned to perform any task, incurring some cost that may vary
-- depending on the agent-task assignment. It is required to perform all
-- tasks by assigning exactly one agent to each task in such a way that
-- the total cost of the assignment is minimized.
--
-- (From Wikipedia, the free encyclopedia.] 
-- """
-- 
-- """
-- These data correspond to an example from [Christofides].
-- """
--
-- See http://www.hakank.org/minizinc/assignment6.mzn
--
cost6 :: Num a => ([Char], [[a]])
cost6 = ("minimize",  [[13, 21, 20, 12,  8, 26, 22, 11],
                       [12, 36, 25, 41, 40, 11,  4,  8],
                       [35, 32, 13, 36, 26, 21, 13, 37],
                       [34, 54,  7,  8, 12, 22, 11, 40],
                       [21,  6, 45, 18, 24, 34, 12, 48],
                       [42, 19, 39, 15, 14, 16, 28, 46],
                       [16, 34, 38,  3, 34, 40, 22, 24],
                       [26, 20,  5, 17, 45, 31, 37, 43]])
                       


