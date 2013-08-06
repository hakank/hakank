% 
% Soccer puzzle in MiniZinc.
% 
% From the Swedish blog Mattebloggen
% "Math problem for the younger, week 44"
% http://mattebloggen.com/2010/11/matteproblem-for-de-yngre-vecka-44/
% (translated)
% """
% Five soccer team played a tournament where each team played against 
% each other once. A win gave 3 points, a tie 1 point, and 0 point
% for a loss.
% 
% Four of the team got 1, 2, 5, and 7 points. How many points
% did the fifth team get?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 5;
% the given results
array[1..4] of int: res = [1,2,5,7];

% number of points for each team
array[1..n] of var 0..3*n: x;

% the result of each game
array[1..n, 1..n] of var {0,1,3}: matches;


solve satisfy;

constraint
  forall(i in 1..4) (
     x[i] = res[i]
  )
  /\
  forall(i in 1..n) (
    matches[i,i] = 0
  )
  /\
  forall(i, j in 1..n where i != j) (
     (matches[i,j] = 3 <-> matches[j,i] = 0) /\
     (matches[i,j] = 1 <-> matches[j,i] = 1)
  ) 
  /\
  forall(i in 1..n) (
      x[i] = sum(j in 1..n) (matches[i,j])
  )  
;

output [show(x)];