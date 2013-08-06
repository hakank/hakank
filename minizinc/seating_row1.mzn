% 
% Movie theater seating problem in MiniZinc.
% 
% Just for fun, heres is a "minimum code" version of 
%    http://www.hakank.org/minizinc/seating_row.mzn
%
% Problem formulation from
% Fei Dai, Mohit Dhawan, Changfu Wu and Jie Wu
% "On Solutions to a Seating Problem", page 1
% """
% n couples are seated in one row. Each person, except two 
% seated at the two ends, has two neighbors. It is required 
% that each neighbor of person k should either have the 
% same gender or be the spouse of k.
% """
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 3; 
int: m = 2*n;
array[1..m] of var 1..m: x;

solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

constraint
   alldifferent(x)
   /\
   forall(i in 2..m) (
     (x[i] mod 2 = x[i-1] mod 2)
     \/
     exists(j in 1..n) (
        (2*(j-1)+1 = x[i] /\ 2*(j-1)+2 = x[i-1]) 
        \/
        (2*(j-1)+1 = x[i-1] /\ 2*(j-1)+2 = x[i]) 
     )
  )
;


output [
 "x     : " ++ show(x) ++ "\n" 
] ++ ["\n"];