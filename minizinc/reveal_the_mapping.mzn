% 
% Reveal the Mapping problem in MiniZinc.
% 
% From 
% Programming Contest 46 - Reveal the Mapping
% http://cplus.about.com/od/programmingchallenges/a/Programming-Contest-46-explain-rings.htm
% """
% Every four years a certain set of 5 rings springs to prominence and the circles 
% are featured in this month's contest. There are several answers to the problem. 
% Using just the 9 unique digits 1-9, map them to the unknowns (a-i) in the diagram 
% so that the sum of the unknowns in each circle add up to 11.
% 
%    [ The circles contains the following letters
%      Circle 1: A,B     1,2
%      Circle 2: B,C,D   2,3,4
%      Circle 3: D,E,F   4,5,6
%      Circle 4: F,G,H   6,7,8
%      Circle 5: H,I     8,9
%    ]
% 
% For instance in the first circle a and b must add up to 11. Then b+c+d in the 
% next circle etc and every digit is used only once.
% 
% This is a brute force speed contest but you must work out all solutions, not 
% just the first. For instance If a= 10 and b= 1 is part of one solution then another 
% might have h= 10 and i = 1 etc. So there must be at least two solutions. Of 
% course A=1 and B= 10 might also work!
% 
% Thanks to Gary at http://delphiforfun.org who posted this originally.
%
% The Contest
% 
% Look at the rings and the areas occupied by a letter. Each circle has two or three 
% letters. Work out the values of all the letters such that in every circle the 
% total of the letters is 11. Then do it for all possible solutions.
%
% Output File
% 
% Please write the different solutions like this into a results file called 
% results.txt in the same location as your exe:
% 
%     Answer 1
%     A= value
%     B=
%     C=
%     ...
%     Answer 2
%     A= value
%     ...
%     Answer 3 etc
%     ... 
%
% Followed by the average time to solve this. If it's very fast, run it a 
% few thousand or so times so that it takes a few seconds to run. Average time = number of runs/time.
% 
% Winning the Contest
% 
% The fastest wins, assuming you get all the answers correct!
% Timing Code
% 
% Please time from the start of the run until just before the output file is 
% opened for writing. This code below will do high speed timing for Windows 
% (first three) and the fourth one for Linux. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: num_circles = 5;
array[1..num_circles] of set of int: circles = [{1,2},
                                                {2,3,4},
                                                {4,5,6},
                                                {6,7,8},
                                                {8,9}];
int: num_letters = 9; 
int: total = 11;

% decision variables
array[1..num_letters] of var 1..num_letters: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   alldifferent(x) 
   /\
   forall(i in 1..num_circles) (
     sum(j in circles[i]) (x[j]) = total
   )
;

output [
  show(x)
]
 ++ ["\n"]
;

