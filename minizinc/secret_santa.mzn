% 
% Secret Santa problem in MiniZinc.
% 
% From Ruby Quiz Secret Santa
% http://www.rubyquiz.com/quiz2.html
% """
% Honoring a long standing tradition started by my wife's dad, my friends 
% all play a Secret Santa game around Christmas time. We draw names and 
% spend a week sneaking that person gifts and clues to our identity. On the 
% last night of the game, we get together, have dinner, share stories, and, 
% most importantly, try to guess who our Secret Santa was. It's a crazily 
% fun way to enjoy each other's company during the holidays.
% 
% To choose Santas, we use to draw names out of a hat. This system was 
% tedious, prone to many "Wait, I got myself..." problems. This year, we 
% made a change to the rules that further complicated picking and we knew 
% the hat draw would not stand up to the challenge. Naturally, to solve 
% this problem, I scripted the process. Since that turned out to be more 
% interesting than I had expected, I decided to share.
% 
% This weeks Ruby Quiz is to implement a Secret Santa selection script.
% 
% Your script will be fed a list of names on STDIN. 
% ...
% Your script should then choose a Secret Santa for every name in the list. 
% Obviously, a person cannot be their own Secret Santa. In addition, my friends 
% no longer allow people in the same family to be Santas for each other and your 
% script should take this into account.
% """
%
% Comment: Well, this model skips the file input and mail parts. We 
%          assume that the friends are identified with a number from 1..n,
%          and the families is identified with a number 1..num_families. 
%
%
%
% Example solution:  [5, 6, 7, 8, 1, 2, 3, 4, 11, 12, 9, 10]
%
% The minizinc solver gives the following as first answer (slightly edited):
%   Person  1 (family: 1) is a Secret Santa of 10 (family: 3)
%   Person  2 (family: 1) is a Secret Santa of  9 (family: 3)
%   Person  3 (family: 1) is a Secret Santa of  8 (family: 3)
%   Person  4 (family: 1) is a Secret Santa of  5 (family: 2)
%   Person  5 (family: 2) is a Secret Santa of 12 (family: 4)
%   Person  6 (family: 3) is a Secret Santa of  4 (family: 1)
%   Person  7 (family: 3) is a Secret Santa of  3 (family: 1)
%   Person  8 (family: 3) is a Secret Santa of  2 (family: 1)
%   Person  9 (family: 3) is a Secret Santa of  1 (family: 1)
%   Person 10 (family: 3) is a Secret Santa of 11 (family: 4)
%   Person 11 (family: 4) is a Secret Santa of  7 (family: 3)
%   Person 12 (family: 4) is a Secret Santa of  6 (family: 3)
%

%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 12;
% int: n = 7; % For the Ruby Quiz example
int: num_families = 4;

array[1..n] of 1..num_families: family = [1,1,1,1, 2, 3,3,3,3,3, 4,4];
% Ruby Quiz example:
% array[1..n] of 1..num_families: family = [1,1,2,2, 3, 4,4];

% decision variables
array[1..n] of var 1..n: x;


% Ensure that there are no fix points in the array.
predicate no_fix_points(array[int] of var int: x) =
  forall(i in index_set(x)) ( 
     x[i] != i
  )
;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % Everyone gives and receives a Secret Santa
  all_different(x) 

  /\ % Can't be one own's Secret Santa
  no_fix_points(x) 

  /\ % No Secret Santa to a person in the same family
  forall(i in index_set(x)) (
     family[i] != family[x[i]]
  )
; 


output [
   "Person " ++ show(i) ++ " (family: " ++ show(family[i]) ++ ") is a Secret Santa of " ++ show(x[i]) ++ " (family: " ++ show(family[x[i]]) ++ ")\n"
   | i in 1..n
] ++ 
["\n"];