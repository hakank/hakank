% 
% Binary puzzle in MiniZinc.
% 
% From
% http://stackoverflow.com/questions/27421360/binary-puzzle-check
% """
% The rules are:
%
%  - Every cell must be a 1 or 0
%  - It's not allowed to get 3 of the same values next to each other or 
%    under/above each other.
%  - Every row and column needs to have a even numbers of 1 and 0, 
%    so in case of a 6 by 6 board, a row/column must have 3 1s and 3 0s
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

%     
% For n = 6, there are 11222 different solutions.
%

include "globals.mzn"; 

int: n = 6;
int: m = n div 2;

% decision variables
array[1..n, 1..n] of var 0..1: x;


% solve satisfy;
solve :: int_search(array1d(x), first_fail, indomain_split, complete) satisfy;

constraint
  forall(i in 1..n) (
     let {
        array[int] of var 0..1: this_row    = [x[i,j] | j in 1..n],
        array[int] of var 0..1: this_column = [x[j,i] | j in 1..n]
     } in 
     % same number of 0..1 in rows and columns
     % count(this_row,1) = count(this_row,0) /\
     % count(this_column,1) = count(this_column,0)
     sum(this_row) = m /\
     sum(this_column) = m

     /\ 
     % no 3 0's or 1's in a row/column
     % require that the sum of the 3-slices of a row/column is either 1 or 2
     sliding_sum(1,2,3,this_row) /\
     sliding_sum(1,2,3,this_column)
  )

  % symmetry breaking
  % /\ x[1,1] = 0
  
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];

