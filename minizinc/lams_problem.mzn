% 
% Lam's Problem in MiniZinc.
%
% http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob025/prob025.pdf% 
% 
% From
% http://www.cse.unsw.edu.au/~tw/csplib/prob/prob025/
% """
% Consider a 111 by 111 binary matrix. The goal is to put 11 zeros in each 
% row in such a way that each column has 11 zeros, and each pair of rows 
% must have exactly one zero in the same column.
% 
% This problem is equivalent to finding a projective plane of order 10. 
% It is also equivalent to the <111,111,11,11,1> BIBD problem (prob028).
% 
% ...
% Results
% """
% The problem has no solution. This shows that finite projective planes of 
% order 10 do not exist.
% 
% The proof caused some controversy as it used computer brute force 
% (2,000 hours on a Cray) and no one has been able (or is ever likely to 
% be able) to check it by hand. Can constraint satisfaction algorithms 
% reduce the size of this task?
% 
% Browne, M. W. "Is a Math Proof a Proof If No One Can Check It?" 
% New York Times, Sec. 3, p. 1, col. 1, Dec. 20, 1988.
% 
% Petersen, I. "Search Yields Math Proof No One Can Check." Science 
% News 134, 406, Dec. 24 & 31, 1988. 
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 11;
var 0..n: m; % = 3;
array[1..n, 1..n] of var 0..1: x;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n] ++ [m], input_order, indomain_max, complete) satisfy;

constraint
%  m <= 4
%  /\
  % rows 
  forall(i in 1..n) (
    sum(j in 1..n) (bool2int(x[i,j] = 0)) = m
  )
  /\ % columns
  forall(j in 1..n) (
    sum(i in 1..n) (bool2int(x[i,j]= 0)) = m
  )
  /\ % pair of rows have exactly one 0 in same column
  forall(i, j in 1..n where i != j) (
     sum(k in 1..n) (bool2int(x[i,k] = x[j,k] /\ x[j,k] = 0 )) = 1
  )
;


output 
[
  "m: ", show(m)
] ++
[
 if j = 1 then "\n" else " " endif ++
   show(x[i,j])
 | i,j in 1..n
] ++ ["\n"]
;