% 
% Nice discount puzzle in MiniZinc.
%

% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 11.
% Source:  J. & L. Poniachik, Hard-to-Solve Brainteasers (p16), Sterling
%
% This model was inspired by the AMPL model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s11.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Note: This MiniZinc model gives no solution with either of the solvers
%       as of 2008-07-03.
% The original AMPL model gives the following answer (with the bonmin solver):
%   t = [0, 30, 80, 40, 0]
%   totcost = 144.4
%   x:
%   0 0 1 0 0
%   0 0 1 0 0
%   0 1 0 0 0
%   0 0 0 1 0
%   0 0 1 0 0

int: m = 5;    % days
set of int: M = 1..m;
set of int: Q = 2..m;
array[M] of int: cost;  % cost of books

array[M, M] of var 0..1: x; % x[i,j]=1 if book i bought on day j, 0 otherwise
array[M] of var 0..100: t; % total cost of books on day j

var 0.0..200.0: totcost = int2float(sum(i in M) (cost[i])) - sum(k in Q) (0.001*int2float(t[k-1])*int2float(t[k]));

% solve :: int_search([x[i,j] | i, j in M], "first_fail", "indomain", "complete") minimize totcost;
solve minimize totcost;

constraint
  forall(j in M) (
        t[j] = sum(i in M) (cost[i]*x[i,j])
  )

  /\
  forall(i in M) (
        sum(j in M) (x[i,j]) = 1
  )

;

output 
[ 
  "t: ", show(t), 
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in M
];

%
% data
%
cost = [10, 20, 30, 40, 50];
