%
% Finite element in MiniZinc.
%
% Marriott & Stuckey Programming in Constraint, page 191ff,
% Temperature in a grid
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

array[1..6,1..7] of var float: grid = 
[|0.0, 100.0, 100.0, 100.0, 100.0, 100.0, 0.0,
 |0.0,   _,       _,     _,     _,     _, 0.0,
 |0.0,   _,       _,     _,     _,     _, 0.0,
 |0.0,   _,       _,     _,     _,     _, 0.0,
 |0.0,   _,       _,     _,     _,     _, 0.0,
 |0.0, 0.0,     0.0,   0.0,    0.0,  0.0, 0.0
|];


% solve :: float_search([grid[i,j] | i in 1..6, j in 1..7], 0.0001, "input_order", "indomain_split", "complete") satisfy;
solve satisfy;

constraint
   forall(i in 2..5, j in 2..6) (
       grid[i,j] = 
            (grid[i,j-1] + grid[i-1,j] + grid[i+1,j] + grid[i,j+1]) / 4.0
       /\
       grid[i,j] >= 0.0 % detta gör att det blir No solution i minizinc
       
   )

;

output [
  if j = 1 then "\n" else " " endif ++
    show(grid[i,j])
  | i in 1..6, j in 1..7
];