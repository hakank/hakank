%
% Some puzzle in Minizinc
%
% (Since I don't know the name of this puzzle, I call it col sum puzzle)
%
% Given the column, row and diagonal sum and some hints, find out
% the rest of the numbers in the matrix.
%
% Note: There are 16 solutions...
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

int: n = 4;
array[1..n] of int: rowsums;
array[1..n] of int: colsums;
array[1..n div 2] of int: diagsums;
array[1..n, 1..n] of int: clues;
array[1..n, 1..n] of var 1..9: x; % decision variables

solve satisfy;

%
% make the clues
%
constraint
  forall(i in 1..n, j in 1..n where clues[i,j] > 0) (
            clues[i,j] = x[i,j]
        )
;

constraint
    % rowsum
    forall(i in 1..n) ( 
       sum(j in 1..n) (x[i,j]) = rowsums[i]
    )
    /\ % colsum
    forall(j in 1..n) (
       sum(i in 1..n) (x[i,j]) = colsums[j]
    )
    /\ % diagonal 1
    sum(i in 1..n) (x[i,i]) = diagsums[1]
    /\ % diagonal 2
    sum(i in 1..n) (x[i,n-i+1]) = diagsums[2]
;


%
% data
%
rowsums = [22, 26, 31, 25];
colsums = [24, 18, 31, 31];
diagsums = [24, 24];

%
% the clues (> 0)
%
clues = array2d(1..n, 1..n, [
  5, 0, 0, 0,
  0, 0, 8, 0,
  0, 0, 0, 8,
  0, 7, 0, 0
])
;

output [
    show(x), "\n",
    show(x[1,1]),"  ", show(x[1,2]), "  ", show(x[1,3]),"  ", show(x[1,4]), " = ", show(rowsums[1]), "\n",
    show(x[2,1]),"  ", show(x[2,2]), "  ", show(x[2,3]),"  ", show(x[2,4]), " = ", show(rowsums[2]),  "\n",
    show(x[3,1]),"  ", show(x[3,2]), "  ", show(x[3,3]),"  ", show(x[3,4]), " = ", show(rowsums[3]),  "\n",
    show(x[4,1]),"  ", show(x[4,2]), "  ", show(x[4,3]),"  ", show(x[4,4]), " = ", show(rowsums[4]),  "\n",
    show(colsums[1]), " ", show(colsums[2]), " ", show(colsums[3]), " ", show(colsums[4]), "\n"
]
