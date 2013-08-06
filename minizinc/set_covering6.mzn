% 
% Set covering in MiniZinc.
% 
% This is a rather general MiniZinc model for the following problem.
%
% From Nathan Brixius
% "Don't be a hero when trying to solve set covering problems"
% http://blogs.msdn.com/b/natbr/archive/2010/06/19/don-t-be-a-hero-when-trying-to-solve-set-covering-problems.aspx
% """
% A colleague posted an interesting question to an internal discussion 
% board the other day:
% 
% Given a set of code blocks changed between two versions of the same 
% binary, and approximated code coverage for the new binary, what are 
% the optimal tests that cover all changed part of the new binary and 
% are least costly to run?
% 
% Example: (A "1" in a cell (Bi, Tj) means that block i is tested by test j)
%     	B1 	B2 	B3 	B4 	B5 	B6 	Cost
% T1 	1 	1 	1 	1 	1 	1 	50
% T2 	0 	1 	1 	1 	1 	1 	20
% T3 	1 	1 	1 	0 	0 	0 	15
% T4 	0 	0 	0 	1 	1 	1 	15
%
% The optimal solution is T3, T4 with minimized cost 15 + 15 = 30. 
% The best greedy solution that we have is T2, T3 with cost 20 + 15 = 35.
%
% The colleague who posted this question also posted a mathematical 
% formulation for the problem:
%
% Binary integer problem statement:
%
% One constraint for each binary block that needs to be covered to 
% make sure we archive maximum coverage. One variable for each test 
% in the list that we try to optimize.
%
% Block 1:   X1 + X3 > 0
% Block 2:   X1 + X2 + X3 > 0
% Block 3:   X1 + X2 + X3 > 0
% Block 4:   X1 + X2 + X4 > 0
% Block 5:   X1 + X2 + X4 > 0
% Block 6:   X1 + X2 + X4 > 0
%
% X1, X2, X3, X4 = {0, 1}
%
% Cost function to optimize: 50*X1 + 20*X2 + 15*X3 + 15*X4
% """
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: num_t;
int: num_b;
array[1..num_t] of int: t_costs;

array[1..num_t, 1..num_b] of 0..1: mat;

% decision variables
array [1..num_t] of var 0..1: x;
var 0..sum(t_costs): total_cost;


% solve satisfy;
% solve minimize total_cost;
solve :: int_search(x, largest, indomain_min, complete) minimize total_cost;

constraint
   % for solve satisfy
   % total_cost = 30 /\

   forall(j in 1..num_b) (
      sum(i in 1..num_t) (x[i]*mat[i,j]) >= 1
   )
   /\
   total_cost = sum(i in 1..num_t) (x[i]*t_costs[i])
;


%
% data (from Brixius blog post)
%
num_t = 4;
num_b = 6;
t_costs = [50,20,15,15];
mat = array2d(1..num_t, 1..num_b, 
[
1,1,1,1,1,1,
0,1,1,1,1,1,
1,1,1,0,0,0,
0,0,0,1,1,1
]);


output
[
  "x: " ++ show(x) ++ "\n" ++
  "total_cost: " ++ show(total_cost) ++ "\n" 

];