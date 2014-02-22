% 
% Sumbrero in MiniZinc.
% 
% http://www.omegajunior.net/code/sumbrero/
% 
% This model was inspired by the discussion in comp.lang.forth:
% https://groups.google.com/forum/#!msg/comp.lang.forth/ZaqK8QgroMI/ZhJyVWCz_XYJ

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: rows;
int: cols;
array[1..rows,1..cols] of int: a;
array[1..rows] of int: sums;
bool: use_alldiff;

% decision variables
array[1..cols] of var 1..9: x;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i in 1..rows) (
     sum([x[j]*a[i,j] | j in 1..cols]) = sums[i]
  )
  /\
  if use_alldiff then
    alldifferent(x)
  else
    true
  endif
;

output [
  show(x)
];


%
% data
%
% Sumbrero 7
% http://www.omegajunior.net/code/sumbrero/

%% Problem instance from
%% https://groups.google.com/forum/#!msg/comp.lang.forth/ZaqK8QgroMI/ZhJyVWCz_XYJ
rows = 9;
cols = 7;
a = array2d(1..rows,1..cols,
[
 % 1 2 3 4 5 6 7
 % A B C D E F G
   1,0,0,0,0,1,0, % 1
   0,1,0,1,0,0,1, % 2
   0,0,1,0,1,0,0, % 3
   1,0,0,1,0,0,0, % 4
   0,1,0,0,1,1,0, % 5
   0,0,1,0,0,0,1, % 6
   0,0,0,1,1,0,0, % 7
   1,1,1,0,0,0,0, % 8
   0,0,0,0,0,1,1, % 9
]);
sums = [8,14,13,3,16,16,8,11,16];
use_alldiff = false;



%% Problem instance from
%% https://groups.google.com/forum/#!msg/comp.lang.forth/ZaqK8QgroMI/ZhJyVWCz_XYJ
%% http://www.omegajunior.net/code/sumbrero/examples.html, Example 1
%% "Example 1 'Different numbers from 1 to 7'"
% rows = 9;
% cols = 7;
% a = array2d(1..rows,1..cols,
% [
%  % 1 2 3 4 5 6 7
%  % A B C D E F G
%    1,0,0,0,0,1,0, % 1
%    0,1,0,1,0,0,1, % 2
%    0,0,1,0,1,0,0, % 3
%    1,0,0,1,0,0,0, % 4
%    0,1,0,0,1,1,0, % 5
%    0,0,1,0,0,0,1, % 6
%    0,0,0,1,1,0,0, % 7
%    1,1,1,0,0,0,0, % 8
%    0,0,0,0,0,1,1, % 9
% ]);
% sums = [9,12,7,4,12,12,3,12,13];
% use_alldiff = true;

