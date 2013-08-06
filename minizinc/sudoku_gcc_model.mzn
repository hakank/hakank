% 
% Sudoku using global cardinality in MiniZinc.
%
% The standard way of solving Sudoku is using the alldifferent constraint
% on rows, columns, and regions.
% 
% Here we instead use of constraint global_cardinality:
% http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html
% 
% 
% For a discussion about this approach see my blog post:
% "Solving Pi Day Sudoku 2009 with the global cardinality constraint"
% http://www.hakank.org/constraint_programming_blog/2009/03/solving_pi_day_sudoku_2009_wit.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% This is the model used by the 89 problems.
% http://www.hakank.org/minizinc/sudoku_gcc_p0.mzn
% ...
% http://www.hakank.org/minizinc/sudoku_gcc_p88.mzn
% 
% They are all from Gecode's Sudoku model sudoku.cpp .
%

include "globals.mzn"; 


int: n;
int: reg = ceil(sqrt(int2float(n))); % size of the regions
array[1..n,1..n] of var 1..n: x;

% the number of occurrences of each number in a row/col/region (always 1)
array[1..n] of 1..1: occ = [1 | i in 1..n];

solve :: int_search([x[i,j] | i, j in 1..n], first_fail, indomain_min, complete) satisfy;


% All different in rows and all different in columns.
%
constraint 


  % global cardinality
  forall(i in 1..n) (
    global_cardinality_old([x[i,j] | j in 1..n], occ)
  )
  /\
  forall(j in 1..n) (
    global_cardinality_old([x[i,j] | i in 1..n], occ)
  ) 
  /\
  forall(i in 0..reg-1,j in 0..reg-1) (
    global_cardinality_old([x[r,c] | r in i*reg+1..i*reg+reg, c in j*reg+1..j*reg+reg] , occ)
  )

%  /\

%   % all_different
%   forall(i in 1..n) (
%     all_different([x[i,j] | j in 1..n])
%   )
%   /\
%   forall(j in 1..n) (
%     all_different([x[i,j] | i in 1..n]) 
%   ) 
%   /\
%   forall(i in 0..reg-1,j in 0..reg-1) (
%     all_different([x[r,c] | r in i*reg+1..i*reg+3, c in j*reg+1..j*reg+reg] )
%   )

;


output 
[
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];
