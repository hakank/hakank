% 
% Scanner problem in MiniZinc.
% 
% Problem from Sverker Jansson
% "AKL - A Multiparadigm Programming Language", page 176
% """
% The problem is to reveal the contents of a grid. Each square may be filled 
% or non-filled. The input is the number of filled squares along each 
% row, column, and diagonal. In general, there may be no, one, or several 
% solutions for a given input. 
% """

%
% The unique solution of the problem instance is a cat:
% 
%                                      *  
%             * * * * * * * * *   * * * *  
%         * * * * * * * * * * * * * * * * *
%         * * * * * * * * * * * * * * * * *
%       * *   * * * * * * * * * * * *      
% *     *     * * * * * * * * * * * *      
% * * * *   * * * * *         * * *        
%           *   * *             * * *      
%           *   * *             * * *      
%           *     * *           *   * *    


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_rows;
int: num_cols;
int: num_diagonal1;
int: num_diagonal2;

array[1..num_rows] of int: rows;
array[1..num_cols] of int: columns;
array[1..num_diagonal1] of int: diagonal1;
array[1..num_diagonal2] of int: diagonal2;

% decision variables
array[1..num_rows, 1..num_cols] of var 0..1: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % rows
  forall(i in 1..num_rows) (
     sum([x[i,j] | j in 1..num_cols]) = rows[i]
     % exactly(rows[i],[x[i,j] | j in 1..num_cols],1)
  )
  /\ % columns
  forall(j in 1..num_cols) (
     sum([x[i,j] | i in 1..num_rows]) = columns[j]
     % exactly(columns[j],[x[i,j] | i in 1..num_rows],1)
  )

  /\ % diagonal1: Upper-left to lower-right diagonals
  forall(k in 1..num_diagonal1) (
     sum([x[i,j] | i in 1..num_rows, j in 1..num_cols where i+j=k+1]) = diagonal1[k]
     % exactly(diagonal1[k], [x[i,j] | i in 1..num_rows, j in 1..num_cols where i+j=k+1], 1)
  )

  /\ % diagonal2: Upper-left to lower-right diagonals
  forall(k in 1..num_diagonal2) (
     sum([x[i,j] | i in 1..num_rows, j in 1..num_cols where i+(num_cols-j+1)=k+1]) = diagonal2[k]
     % exactly(diagonal2[k], [x[i,j] | i in 1..num_rows, j in 1..num_cols where i+(num_cols-j+1)=k+1], 1)
  )

;

output [
  if j = 1 then "\n" else " "  endif ++
    if fix(x[i,j]) = 1 then "*" else " " endif
  | i in 1..num_rows, j in 1..num_cols
];


% 
% data
%
num_rows = 10;
rows = [1,13,17,17,14,14,12,6,6,6];

num_cols = 21;
columns = [2,1,1,3,3,6,6,8,9,7,5,5,5,5,6,8,8,8,4,4,2];

% Upper-left to lower-right diagonals
num_diagonal1 = 30;
diagonal1 = [0,0,0,0,0,1,2,5,6,4,4,6,7,7,8,8,5,6,6,6,6,5,5,3,3,1,1,1,0,0];

% Upper-right to lower-left diagonals 
num_diagonal2 = 30;
diagonal2 = [0,1,2,3,3,3,2,4,5,5,7,9,9,6,6,5,4,5,5,4,5,4,2,2,2,2,1,0,0,0];
