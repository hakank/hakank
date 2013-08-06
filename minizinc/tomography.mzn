% 
% Tomography problem in Minizinc.
% 
% Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
% """
% This is a little "tomography" problem, taken from an old issue
% of Scientific American.
%
% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%?- go.
%    0 0 7 1 6 3 4 5 2 7 0 0
% 0                         
% 0                         
% 8      * * * * * * * *    
% 2      *             *    
% 6      *   * * * *   *    
% 4      *   *     *   *    
% 5      *   *   * *   *    
% 3      *   *         *    
% 7      *   * * * * * *    
% 0                         
% 0                         
%
%
% Eclipse solution by Joachim Schimpf, IC-Parc
% """
% 
% This model is simplified in that here we show 1 for "*" and " " for 0. 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


int: r; % number of rows
int: c; % number of columns
array[1..r] of int: row_sums;
array[1..c] of int: col_sums;

array[1..r, 1..c] of var 0..1: x; % decision variable

solve satisfy;

constraint
     % the rows
     forall(i in 1..r) (
       row_sums[i] = sum([x[i,j] | j in 1..c])
     )
     /\ % the columns
     forall(j in 1..c) (
       col_sums[j] = sum([x[i,j] | i in 1..r])
     )
;


%
% These following three examples are from the ECLiPSe program cited above.
% 
r = 11;
c = 12;
row_sums = [0,0,8,2,6,4,5,3,7,0,0];
col_sums = [0,0,7,1,6,3,4,5,2,7,0,0];
 
% r = 5;
% c = 13;
% row_sums = [10,4,8,5,6];
% col_sums = [5,3,4,0,5,0,5,2,2,0,1,5,1];


% This give three slightly different solutions.
% r = 3;
% c = 11;
% row_sums = [11,5,4];
% col_sums = [3,2,3,1,1,1,1,2,3,2,1];


output [ 
    if j = 1 then  "   " else "" endif ++
    show(col_sums[j]) ++ " "
    | j in 1..c
]  
++
[
   if j = 1 then "\n" ++ show(row_sums[i]) ++ "  "  else " " endif ++
     show(x[i,j])
   | i in 1..r, j in 1..c
]
++ ["\n"]

