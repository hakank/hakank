% 
% No three in line problem in MiniZinc.
% 
% From
% http://en.wikipedia.org/wiki/No-three-in-line_problem
% """
% In mathematics, in the area of discrete geometry, the no-three-in-line-problem, 
% introduced by Henry Dudeney in 1917, asks for the maximum number of points 
% that can be placed in the n × n grid so that no three points are collinear. 
% This number is at most 2n, since if 2n + 1 points are placed in the grid some 
% row will contain three points.
% """
% 
% Optimal values (which is 2*n).
% 
% N   Tot
% =======
% 1   1
% 2   4
% 3   6
% 4   8
% 5  10
% 6  12
% 7  14
% 8  16
% 9  18
% 10 20
% ...
% 100 200

%
% Number of optimal solutions (tot=2*n), 
% Symmetry breaking with lex2/1
%
% N  #solutions
% =============
% 1      0
% 2      1
% 3      1
% 4      2
% 5      5
% 6     13
% 7     42
% 8    155
% 9    636
% 10  2889
% 11 14321

% This sequence:
% 0,1,1,2,5,13,42,155,636,2889,14321
% is the OEIS sequence http://oeis.org/A229161
% """
% Number of n X n binary matrices with exactly 2 ones in each row 
% and column, and with rows and columns in lexicographically nondecreasing order. 
% """

%
% Note: This model does _not_ give the same sequence as on the 
%       Wikipedia page http://en.wikipedia.org/wiki/No-three-in-line_problem
%       i.e. the OEIS sequence https://oeis.org/A000769
%       1, 1, 4, 5, 11, 22, 57, 51, 156, 158, 566, 499, 1366, 
%       And the reason is that the symmetry breaking is different

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
% include "gecode.mzn"; % for afc_min
int: n = 10;
int: m = 3;
array[1..n, 1..n] of var 0..1: x;
var 0..2*n: tot;


% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..n] ++ [tot], 
        first_fail, % size_afc_min, % smallest, % first_fail, % occurrence, 
        indomain_max, 
        complete) 
    satisfy;
    % maximize tot;

constraint
   tot = sum([x[i,j] | i,j in 1..n])
   /\
   forall(i in 1..n) (
      sum([x[a,i] | a in 1..n]) < m /\
      sum([x[i,a] | a in 1..n]) < m
   )
;

constraint tot=2*n;

%
% symmetry breaking
%
constraint 
   lex2(x)
;


output
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j  in 1..n
]
++
[ "\ntot: " ++ show(tot) ++ "\n" ]

;