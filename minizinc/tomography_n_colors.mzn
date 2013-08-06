% 
% Discrete tomography problem with n colors in Minizinc.
% Generalization of 2-colors problem.
% 
% This solutions just use one matrix and codes the different colors
% with different numbers 1..num_colors.
% 
% From
% http://www.lix.polytechnique.fr/~durr/Xray/Complexity/
% """
% Can you toggle the cell-colors by mouse-clicks such that all numbers are zero? 
% The computational complexity of this problem (is it possible to solve the 
% problem efficiently or is it NP-complete) is open since 1997.
% 
% In this problem there are two colors (not counting white indicating an empty 
% cell). However for a single color the problem is easy [5] and can be solved in 
% time O(n log n), where n is the side length of the grid. And for three or 
% more colors the problem is NP-complete [2] . (Previously it was known to be 
% hard for six colors or more [4] .) 
% """
% Note: The problems are changed each time the page is loaded

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: r; % number of rows
int: c; % number of columns
int: num_colors;
array[1..num_colors,1..r] of int: row_sums;
array[1..num_colors,1..c] of int: col_sums;

array[1..r, 1..c] of var 0..num_colors: x; % decision variable

solve :: int_search([x[i,j] | i in 1..r, j in 1..c], input_order, indomain_min, complete) satisfy;
% solve satisfy;

constraint
     % the rows
     forall(i in 1..r) (
       forall(k in 1..num_colors) ( 
          % count how many k's there are.
          % (Since it use bool2int it makes the problem 
          % unaccessable to explex: undef procedure int_eq_reif.)
          row_sums[k,i] = sum(j in 1..c) (bool2int(x[i,j] = k))
       )
     )
     /\ % the cols
     forall(j in 1..c) (
       forall(k in 1..num_colors) ( 
          col_sums[k,j] = sum(i in 1..r) (bool2int(x[i,j] = k))
       )
     )
;

output 
[
    if j = 1 then "\n" else " " endif ++
       show(x[i,j])
    | i in 1..r, j in 1..c
 ] ++ ["\n"]
;


%
% data
% 


% The two atom problem
num_colors = 2; % row 1:blue, row 2:red
c = 10;
col_sums = [|3,3,5,4,2,4,4,2,3,4, 
            |3,4,2,3,3,3,3,6,2,4|]; 
r = 10;
row_sums = [|3,3,4,3,4,3,4,4,4,2, 
            |2,3,5,3,6,2,2,2,2,6|];


% three colors:
% This problem has 5 solutions.
% num_colors = 3;
% c = 5;
% col_sums = [|0,2,1,2,0,
%             |2,0,0,0,2,
%             |1,0,2,0,1|];
% r = 5;
% row_sums = [|0,2,1,2,0,
%             |2,0,0,0,2,
%             |1,0,2,0,1|];


% one dimensional problems (checking)
% the spiral:
% num_colors = 1;
% r = 11;
% c = 12;
% row_sums = [0,0,8,2,6,4,5,3,7,0,0];
% col_sums = [0,0,7,1,6,3,4,5,2,7,0,0];

% 
% the "h" (or chair)
% num_colors = 1;
% r = 14;
% c = 14;
% row_sums = [0,2,2, 2, 2,2,8,8,4,4,4,4,4,0];
% col_sums = [0,0,0,12,12,2,2,2,2,7,7,0,0,0];
