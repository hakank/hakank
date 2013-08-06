% 
% 3 coins problem in MiniZinc.
% 
% From 
% http://www.cs.cf.ac.uk/htbin/Dave/AI/ai.pl?AI1/means_end.html+agenda+game+AI1/AI1.html+Lecture_12:_Agendas+Lecture_14:_Game_Playing+AI_TOP_LEVEL+LECTURE_13:_Means_End_Analysis
% """
% Three coins lie on a table in the order tails, heads ,tails. In precisely three moves 
% make them face either all heads or all tails, GPS generated 10 goals.
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
int: num_moves = 3; % first row is the init move
array[1..num_moves+1,1..n] of var 0..1: x; % 0: head, 1: tail
array[1..n] of 0..1: init = [1,0,1];
var 0..n: last_sum;
 
solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

    forall(i in 1..n) (
        x[1,i] = init[i]
    )
    /\
    forall(m in 2..num_moves+1) (
       sum(i in 1..n) (bool2int(x[m-1,i] != x[m,i])) = 1 % one diff per move
    )
    /\
    last_sum = sum(i in 1..n) ( x[num_moves+1,i])

    /\
    (
    % either all heads or all tails
    last_sum = 0 \/ last_sum = n
    )

    % /\
    % all_equal([x[num_moves+1, i] | i in 1..n])

;

output[
  if j = 1 then "\n" else " " endif ++
  show(x[m,j]) 
  | m in 1..num_moves+1, j in 1..n
] ++ 
["\n"];


