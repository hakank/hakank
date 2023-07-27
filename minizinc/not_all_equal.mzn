% 
% Global constraint not_all_equal in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnot_all_equal.html
% """
% Constraint
%
%     not_all_equal​(VARIABLES)​
%
% Purpose
%
%     The variables of the collection VARIABLES should take more 
%     than one single value.
%
% Example
%     ​(<3, 1, 3, 3, 3>)
%
%     The not_all_equal constraint holds since the collection 
%     <3, 1, 3, 3, 3> involves more than one value (i.e., 
%     values 1 and 3).
%% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..3: variables;

predicate not_all_equal(array[int] of var int: variables) =
   let {
     var min(index_set(variables))..max(index_set(variables)): nval 
   } in
   nvalue(nval, variables)
   /\
   nval >= 2
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
   cp1d(variables, [3,1,3,3,3]) /\
   not_all_equal(variables)
;


output
[
  show(variables)
];
