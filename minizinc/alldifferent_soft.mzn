% 
% Global constraint soft all_different in MiniZinc.
% 
%   * soft_all_different_ctr
%   * soft_all_different_var
%
% soft_all_different_ctr: 
% https://sofdem.github.io/gccat/gccat/Csoft_all_different_ctr.html
% 
% """
% Consider the disequality constraints involving two distinct variables of the collection 
% VARIABLES. Among the previous set of constraints, C is the number of disequality constraints 
% that do not hold.
% 
% Example
% (4, <5, 1, 9, 1, 5, 5>)
% 
% Within the collection <5, 1, 9, 1, 5, 5> the first and fifth values, the first and sixth values, 
% the second and fourth values, and the fifth and sixth values are identical. Consequently, 
% the argument C = 4 is fixed to the number of disequality constraints that do not hold (i.e, 4) 
% and the soft_alldifferent_ctr constraint holds.
% """
% 
%
% soft_alldifferent_var:
% https://sofdem.github.io/gccat/gccat/Csoft_alldifferent_var.html
% """
% C is the minimum number of variables of the collection VARIABLES for which the value 
% needs to be changed in order that all variables of VARIABLES take a distinct value.
% 
% Example
%  (3, <5, 1, 9, 1, 5, 5>)
% 
%     Within the collection <5, 1, 9, 1, 5, 5>, 3 and 2 items are respectively fixed to values 5 
% and 1. Therefore one must change the values of at least (3-1) + (2-1) = 3 items to get back 
% to 6 distinct values. Consequently, the soft_alldifferent_var constraint holds since its first 
% argument C is fixed to 3.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 


int: n = 6;
array[1..n] of var 1..9: x;
var int: a;
var int: b;
var int: d = b - a;

solve satisfy;
% solve maximize d;

%
% all_different_soft_ctr
%
predicate all_different_soft_ctr(var int: diffs, array[int] of var int: x) =
   let {
      int: lbx = min(index_set(x)),
      int: ubx = max(index_set(x))
   } 
   in
   diffs = sum(i in lbx..ubx, j in lbx..i-1) (
        bool2int(x[i] = x[j])
   )
;

%
% all_different_soft_var
%
predicate all_different_soft_var(var int: diffs, array[int] of var int: x) =
   let {
      int: lbx = min(index_set(x)),
      int: ubx = max(index_set(x))
   } 
   in
   diffs = sum(i in lbx..ubx) (
       bool2int(  
         sum(j in lbx..i-1) ( bool2int(x[i] = x[j])  ) > 0
       )
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


constraint
   cp1d([5,1,9,1,5,5],x)  % _var gives 3, _ctr gives 4
   % cp1d([5,5,5,5,5,5],x)  % _var gives 5, _ctr gives 15
   % cp1d([1,1,1,1,3,3],x)
   /\
   all_different_soft_var(a, x)
   /\
   all_different_soft_ctr(b, x)
   % /\ 
   % a < b
   % a = 3
   % /\
   % increasing(x)
;

output 
[
  "x: " ++ show(x) ++ "\n" ++
  "a: " ++ show(a) ++ "\n" ++
  "b: " ++ show(b) ++ "\n" ++
  "d: " ++ show(d) ++ "\n"
];
