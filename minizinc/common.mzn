% 
% Global Constraints 
%
%    common 
%    used_by 
%
% in MiniZinc.
% 
% From Global Constraint Catalogue:
% 
% common:
% https://sofdem.github.io/gccat/gccat/Ccommon.html
% """
% NCOMMON1 is the number of variables of the collection of variables 
% VARIABLES1 taking a value in VARIABLES2.
%
% NCOMMON2 is the number of variables of the collection of variables 
% VARIABLES2 taking a value in VARIABLES1.
%
% Example:
% common(3, 4, <1, 9, 1, 5>, <2, 1, 9, 9, 6, 9>)
%
% The common constraint holds since:
%  * Its first argument NCOMMON1 = 3 corresponds to the number of values of the 
%    collection <1, 9, 1, 5> that occur within <2, 1, 9, 9, 6, 9>.
%  * Its second argument NCOMMON2 = 4 corresponds to the number of values of 
%    the collection <2, 1, 9, 9, 6, 9> that occur within <1, 9, 1, 5>.
%
% """
% 
% used_by:
% https://sofdem.github.io/gccat/gccat/Cused_by.html
% """
% All the values of the variables of collection VARIABLES2 are used by 
% the variables of collection VARIABLES1.
% 
% Example: used_by([1, 9, 1, 5, 2, 1], [1, 1, 2, 5])
% """"

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 


array[1..4] of var 1..9: x;
array[1..6] of var 1..9: y;

var int: a;
var int: b;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

%
% helper predicate to convert an array to a set
%
predicate array2set(array[int] of var int: x, var set of int: s) =
   let { 
      int: n = length(x)
   }
   in
   forall(i in index_set(x)) (  x[i] in s ) /\
   % secure that the values _not_ in x is _not_ in s
   forall(i in lb_array(x)..ub_array(x)) (
     (not exists(j in index_set(x)) (x[j] = i ) <-> not (i in s))
   )
;


%
% common(a, b, x, y)
%  - a is the number of values of x that are in y
%  - b is the number of values of y that are in x
%
predicate common(var int: a, var int: b, array[int] of var int: x, array[int] of var int: y) = 
   let {
     var set of lb_array(x)..ub_array(x): x_set,
     var set of lb_array(y)..ub_array(y): y_set
   }
   in
   array2set(x, x_set) /\
   array2set(y, y_set) /\
   a = sum(i in index_set(x)) (
           bool2int(x[i] in y_set)
       )
   /\
   b = sum(j in index_set(y) ) (
           bool2int(y[j] in x_set)
       )

;

%
% used_by(x, y)
% 
% _All_ values in x are in y..
%
predicate used_by(array[int] of var int: x, array[int] of var int: y) =
   let {
     var set of lb_array(y)..ub_array(y): y_set,
   }
   in
   array2set(y, y_set) /\
   length(x) = sum(i in index_set(x)) (
           bool2int(x[i] in y_set)
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
   cp1d([1,9,1,5],x) /\
   cp1d([2,1,9,9,6,9],y) /\

   common(a, b, x, y)

   % The "reverse problem" works as well, i.e. letting x and y be unknown
   % and fix a and b.
   % /\ a = 3
   % /\ b = 4

   % /\ increasing(x)
   % /\ increasing(y) 

   % /\ 
   % used_by(x, y)
   % /\ used_by(y, x)
;



 output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "a: " ++ show(a) ++ "\n" ++
  "b: " ++ show(b) ++ "\n" 
];
