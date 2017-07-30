% 
% Sudoku using alldifferent in MiniZinc.
%
% This model solves Sudoku problems using the all_different constraint.
%
% It can be seen as a companion model to
%   http://www.hakank.org/minizinc/sudoku_gcc_model.mzn
% with the (not so common) approach of using global cardinality count (gcc).
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
% http://www.hakank.org/minizinc/sudoku_problems2/sudoku_p0.dzn
% ...
% http://www.hakank.org/minizinc/sudoku_p88.dzn
% 
% These problems are all from Gecode's Sudoku solver sudoku.cpp .
%

include "globals.mzn"; 

% Remove the comment is using Gecode/fz. Also use the
% int_search with size_afc_min (see below).
% include "gecode.mzn";

int: n;
int: reg = ceil(sqrt(int2float(n))); % size of the regions
array[1..n, 1..n] of var 1..n: x;

% If using Gecode: 
%    size_afc_min is quite fast, especially for larger problems, e.g. 
%    problem 20 (size 25x25) 
% solve :: int_search([x[i,j] | i, j in 1..n], size_afc_min, indomain_min, complete) satisfy;

solve :: int_search([x[i,j] | i, j in 1..n], first_fail, indomain_min, complete) satisfy;

%
% All different in rows and all different in columns.
%
constraint 

  % Show the size of the problem.
  % trace("n: " ++ show(n) ++ "\n", 1=1) /\

  % global cardinality
  forall(i in 1..n) (
    alldifferent([x[i,j] | j in 1..n]) % :: domain
  )
  /\
  forall(j in 1..n) (
    alldifferent([x[i,j] | i in 1..n])  :: domain
  )   /\
  forall(i in 0..reg-1,j in 0..reg-1) (
    alldifferent([x[r,c] | r in i*reg+1..i*reg+reg, c in j*reg+1..j*reg+reg])  % :: domain
  )

;

output 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];
