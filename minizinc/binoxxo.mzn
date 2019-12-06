% 
% Binoxxo problem in MiniZinc.
% 
% From https://github.com/AdrianKauz/HSLU_FS19_AISO_Exercises/blob/62088b7b67477cadc936354ebb48c89dd33dc49b/Constraint_Programming/BinoxxoGenerator.ipynb
% """
% Module: HSLU - Artificial Intelligence - Search & Optimization (AISO)
% Source: Slides "Constraint programming 1 - Modelling with OR-Tools"
% Author: Adrian Kauz
% 
% Place X or O in the empty cells so that there are no more than
% two consecutive X's or O's in a row or a colum. The number of X's
% is the same as the number of O's in each row and column. All
% rows and columns are unique.
% More precisely, the rows must be different from each other and
% separately, the columns must be differ from each other.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%
include "globals.mzn";

% """
% Size of the board should be an even number. Read the rules then you know why.
% """
int: n = 14;
int: m = n div 2; % number of X's in each row/column

% decision variables
array[1..n,1..n] of var 0..1: x;

constraint
   % Place X or O in the empty cells so that there are no more
   % than two consecutive X's or O's in a row or a colum.
   % (I.e. for each seqment of 3 there can be max two 1's (sum is 0..2).
   forall(i in 1..n) (
                      % Max 2 1's in a row
                      sliding_sum(0,2,3,[x[i,j] | j in 1..n]) /\
                      sliding_sum(0,2,3,[x[j,i] | j in 1..n])
                      
                      /\ % Max 2 0's in a row
                      forall(j in 1..n-2) (

                                          sum([x[i,j+k] = 0 | k in 0..2]) <= 2 /\
                                          sum([x[j+k,i] = 0| k in 0..2]) <= 2
                                          )
                   )

   /\
   % The number of X's is the same as the number of O's in each row and column.
   % (I.e. the sum of each row/column is n / 2.
   forall(i in 1..n) (
                      sum([x[i,j] | j in 1..n]) = m /\
                      sum([x[j,i] | j in 1..n]) = m 
                      )
   /\
   % All rows and columns are unique.
   forall(i,j in 1..n where i < j) (
                     sum([x[i,k] != x[j,k]  | k in 1..n]  ) > 0 /\
                     sum([x[k,i] != x[k,j]  | k in 1..n]  ) > 0
                     )
;


solve satisfy;
% solve :: int_search(array1d(x), input_order, indomain_max, complete) satisfy;


output [
        if j = 1 then "\n" else "" endif ++
        if fix(x[i,j]) = 0 then "O" else "X" endif
        | i,j in 1..n
];

