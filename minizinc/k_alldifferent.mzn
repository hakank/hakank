% 
% Global constraint k-alldifferent in MiniZinc.
%
% https://sofdem.github.io/gccat/gccat/Ck_alldifferent.html
% """
% For each collection of variables depicted by an item of VARS, enforce their corresponding 
% variables to take distinct values.
% 
% Example
% (
% <
%   vars-<5, 6, 0, 9, 3>,​
%   vars-<5, 6, 1, 2>
% >
% )
%
% The k_alldifferent constraint holds since all the values 5, 6, 0, 9 and 3 are distinct and 
% since all the values 5, 6, 1 and 2 are distinct as well.

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 


int: n = 2;
int: m = 5;

array[1..n, 1..m] of var 0..9: x;

%
% k_all_different
% (all rows satisfy the all_different constraint)
%
predicate k_all_different(array[int, int] of var int: x) =
    forall(i in index_set_1of2(x)) (
     all_different([x[i,j] | j in index_set_2of2(x)]) 
  )
;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
      forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
     ) 
  )
; 


solve satisfy;

constraint
  cp2d(x,[|5,6,0,9,3,
       |5,6,1,2,_|])
  /\ 
  k_all_different(x)
;

output
[
  show(x)
];
