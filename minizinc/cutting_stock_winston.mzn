% 
% Cutting stock example in Minizinc.
%
% Winston "Operations Research", page 570.

% """
% Woodco sells 3-ft, 5-ft, and 9-ft pieces of lumber. W:s customers demand
% 25 3-ft, 20 5-ft and 15 9-ft boards. ... by cutting up 17-ft boards.
% """

% The different ways of cutting a 17-ft board:
               
% Combination  3-ft   5-ft   9-ft   waste
% 1             5     0      0       2
% 2             4     1      0       0
% 3             2     2      0       1
% 4             2     0      1       2
% 5             1     1      1       0
% 6             0     3      0       2
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_cuts = 3;
array[1..num_cuts] of int:  cuts;  
int: num_comb = 6;
array[1..num_comb, 1..num_cuts] of int: combinations;
array[1..num_cuts] of int: demand;
array[1..num_comb] of var int: x;
var int: total_sum = sum(i in 1..num_comb) (x[i]);

solve minimize total_sum;
% solve satisfy;

constraint
  forall(i in 1..num_comb) (
    x[i] >= 0
  )
  /\
  forall(j in 1..num_cuts) (
     sum(i in 1..num_comb) 
       (combinations[i,j]*x[i]) >= demand[j]
  )
;

%
% Data
%
combinations =
array2d(1..num_comb, 1..num_cuts, [
  5, 0, 0,
  4, 1, 0,
  2, 2, 0,
  2, 0, 1,
  1, 1, 1,
  0, 3, 0
]);

cuts = [3,5,9];
demand =  [25, 20, 15];

output
[
  "x: " ++ show(x) ++ "\n" ++
  "total_sum: " ++ show(total_sum)
];