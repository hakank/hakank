% 
% Finding minimal subset of columns that make rows in a matrix unique in MiniZinc.
% 
% Finding minimal subset of columns that make rows in a matrix unique
% http://stackoverflow.com/questions/36449631/finding-minimal-subset-of-columns-that-make-rows-in-a-matrix-unique
% """
% What is a generic, efficient algorithm to find the minimal subset of columns in 
% a discrete-valued matrix that makes that rows unique.
% 
% For example, consider this matrix (with named columns):
% 
%  a  b  c  d
%  2  1  0  0
%  2  0  0  0
%  2  1  2  2
%  1  2  2  2
%  2  1  1  0
%
% Each row in the matrix is unique. However, if we remove columns a and d we 
% maintain that same property.
% 
% I could enumerate all possible combinations of the columns, however, that will 
% quickly become intractable as my matrix grows. Is there a faster, 
% optimal algorithm for doing this?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: rows;
int: cols;

array[1..rows,1..cols] of int: m;

% decision variables
array[1..cols] of var 0..1: x; % the selected columns (1: selected)
var 1..cols: z = sum(x); % number of selected columns 

solve minimize z;
% solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(r1,r2 in 1..rows where r1 < r2) (
    sum([ x[j]*(m[r1,j] != m[r2,j]) | j in 1..cols]) >= 1
  )

;

output [
  "z: \(z)\n",
  "x: \(x)\n",
  "rows: \(rows)\ncols: \(cols)\n"
]
++
[
  show([m[r,c] | c in 1..cols where fix(x[c]) = 1]) ++ "\n"
  | r in 1..rows
]
;

% See above
rows = 5;
cols = 4;
m = array2d(1..rows,1..cols,
[
2,1,0,0,
2,0,0,0,
2,1,2,2,
1,2,2,2,
2,1,1,0,
]);

% rows = 9;
% cols = 7;
% m = array2d(1..rows,1..cols,
% [
% 0,1,0,1,0,1,1,
% 0,1,1,2,0,0,2,
% 1,0,1,1,1,0,0,
% 1,2,2,1,1,2,2,
% 2,0,0,0,0,1,1,
% 2,0,2,2,1,1,0,
% 2,1,2,1,1,0,1,
% 2,2,1,2,1,0,1,
% 2,2,2,1,1,2,1
% ]);


% random version
% rows = 70; % uniform(2,5);
% cols = 70; % uniform(5,10);
% m = array2d(1..rows,1..cols,[ uniform(0,5) | i in 1..rows, j in 1..cols ]);
