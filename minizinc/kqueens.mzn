% 
% Kraitchik's queen placement problem in MiniZinc.

% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 9. 
% Description  : Kraitchik's queen placement problem
% Source       : Kraitchik, M., (1942), Mathematical Recreations, W.W. Norton and Company, Inc.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s9.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: size = 8;

set of 1..size: S = 1..size;
array[S, S] of var 0..1: x; % x(i,j) = 1 if occupied, 0 otherwise
array[S, S] of var 0..1: a; % a(i,j) = 1 if attacked, 0 otherwise

var int: numq= sum(i in S,j in S) (x[i,j]);
  
% minimise number of queens
solve :: int_search([ x[i,j] | i, j in S], first_fail, indomain_min, complete) minimize numq;

constraint 
   % a[i,j] = 0 if square (i,j) not attacked
   forall(i in S,j in S) (
        sum(m in S where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
        sum(m in S where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) +
        sum(m in S) (x[m,j]) + 
        sum(n in S where n != j) (x[i,n]) >= a[i,j]
   )
   /\
   % each square either attacked or occupied
   forall(i in S,j in S) (
       a[i,j]+x[i,j] = 1
   )
;  

output [
   if i = 1 /\ j = 1 then
    "\nnumq: " ++ show(numq)
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in S, j in S
] ++ ["\n"];

