% 
% Global constraint decreasing in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cdecreasing.html
% """
% The variables of the collection VARIABLES are decreasing.
%
% Example
%     ​(<8,​4,​1,​1>)​
% 
% The decreasing constraint holds since 8=>4=>1=>1
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..8: x;

predicate decreasing_me(array[int] of var int: x) = 
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x))
   }
   in
   forall(i in lbx+1..ubx) (
     x[i-1] >= x[i] 
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


solve satisfy;

constraint
   cp1d([8,4,1,1],x)
   /\
   decreasing_me(x)
;

output
[
  show(x)
];
