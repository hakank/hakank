% 
% Global constraint atmost_nvalue in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Catmost_nvalue.html
% """
% The number of distinct values taken by the variables of the 
% collection VARIABLES is less than or equal to NVAL.
% 
% Example
%    (4, <3, 1, 3, 1, 6>)
%
% The atmost_nvalue constraint holds since the collection <3, 1, 3, 1, 6> 
% involves at most 4 distinct values (i.e., in fact 3 distinct values).
%
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..6: x;
var int: nval;

%
% helper predicate to convert an array to a set
%
predicate array2set(array[int] of var int: x, var set of int: s) =

   forall(i in index_set(x)) (  x[i] in s ) /\
   % secure that the values _not_ in x is _not_ in s
   forall(i in lb_array(x)..ub_array(x)) (
     (not exists(j in index_set(x)) (x[j] = i ) <-> not (i in s))
   )
;


predicate atmost_nvalue(var int: nval, array[int] of var int: x) =
  let {
    var set of lb_array(x)..ub_array(x): s
  }
  in
  array2set(x, s)
  /\
  card(s) <= nval
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


solve satisfy;

constraint
  cp1d([3, 1, 3, 1, 6], x)
  /\
  atmost_nvalue(nval, x)
  /\
  nval = 4

;


output [show(x)];
