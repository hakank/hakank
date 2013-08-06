% 
% 5x5 puzzle (Five puzzle) in MiniZinc.
%
% From http://www.chlond.demon.co.uk/Five.html (Java applet).
% (Link from http://www.chlond.demon.co.uk/puzzles/puzzles1.html)
% 
% """
% Each of the squares in the above grid can be in one of two states, lit(white)
% or unlit(red). If the player clicks on a square then that square and each 
% orthogonal neighbour will toggle between the two states. Each mouse click 
% constitutes one move and the objective of the puzzle is to light all 
% 25 squares in the least number of moves.
% """
%
% Compare with the following model:
% * MiniZinc: http://www.hakank.org/minizinc/five.mzn
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 5;
% int: n = 11;

set of 1..n: N = 1..n;
array[N,N] of var 0..1: x;
array[N,N] of var 0..n: d;
var int: the_sum = sum(i in N, j in N) (x[i,j]);

constraint
         forall(i in N,j in N) (
                  2*d[i,j]+1 = 
             (sum(k in j-1..j+1 where k >= 1 /\ k <= n /\ k != j) (x[i,k]) + 
                       sum(k in i-1..i+1 where k >= 1 /\ k <= n) (x[k,j])) 
       )
;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in N], first_fail, indomain_min, complete)  minimize the_sum;


output [
   if i = 1 /\ j = 1 then "\nThe_sum: " ++ show(the_sum) else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])  
   | i in N, j in N
];
