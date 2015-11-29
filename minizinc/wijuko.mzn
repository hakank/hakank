% 
% Wijuko grid puzzle in MiniZinc.
% 
% Place 1-9 in the grid, obeying the sums between pairs of squares.
%
% Here's the template, where X is a square and n is a number between 1 and 9.
%
%   X n X n X
%   n   n   n
%   X n X n X
%   n   n   n
%   X n X n X 
%
%
% See Martin J. Chlond: "Puzzle - IP in the I"
% http://pubsonline.informs.org/doi/pdf/10.1287/ited.2015.0142
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%
include "globals.mzn"; 

int: n = 3;
int: m = 5;

array[1..m,1..n] of var int: d;

% decision variables
array[1..n,1..n] of var 1..9: x;


solve satisfy;
% solve :: int_search(array1d(x), first_fail, indomain_min, complete) satisfy;

constraint
   all_different(array1d(x)) /\

   % first row
   % x[1,1] + x[1,2] = d[1,1] /\
   % x[1,2] + x[1,3] = d[1,2] /\

   % x[1,1] + x[2,1] = d[2,1] /\
   % x[1,2] + x[2,2] = d[2,2] /\
   % x[1,3] + x[2,3] = d[2,3] /\

   % second row
   % x[2,1] + x[2,2] = d[3,1] /\
   % x[2,2] + x[2,3] = d[3,2] /\

   % x[2,1] + x[3,1] = d[4,1] /\
   % x[2,2] + x[3,2] = d[4,2] /\
   % x[2,3] + x[3,3] = d[4,3] % /\

   % third row
   % x[3,1] + x[3,2] = d[5,1] /\
   % x[3,2] + x[3,3] = d[5,2]

   %
   % more general
   % 
   % row constraints
   forall(i in 1..n, j in 1..2) (
     x[i,j] + x[i,j+1] = d[(i-1)*2+1,j] 
   )

   /\ % column constraints
   forall(i in 1..2,j in 1..n) (
     x[i,j] + x[i+1,j] = d[(i-1)*2+2,j]
   )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];


%
% Problem instance from Chlond's article:
%
%    X  _  X  7  X
%
%   12     8    13
%
%    X  _  X 14  X
%
%    7     _    15
%
%    X  _  X  _  X 

%
%
d = 
array2d(1..m, 1..n,
[
  _,  7, 0,  % row
 12,  8,13,  % column
  _, 14, 0,  % row
  7, _, 15,  % column
  _, _,  0   % row
]);


% another example:
% http://www.pressreader.com/search?query=wijuko
% Solution:
%
%  4 9 1
%  8 2 3
%  5 7 6
% 
% d = 
% array2d(1..m, 1..n,
% [
%   13, _, 0,  % row
%   12,11, _,  % column
%    _, _, 0,  % row
%    _, _, 9,  % column
%   12,13, 0   % row
% ]);
%