% 
% Global constraint not_in in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnot_in.html
% """
% Constraint
%
%     not_in​(VAR,​VALUES)​
%
% Purpose
%
%     Remove the values of the VALUES collection from the domain variable VAR.
%
% Example
%     ​(2, <1, 3>)
%
%     The constraint not_in holds since the value of its first argument 
%     VAR=2 does not occur within the collection <1, 3>.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 2;
array[1..n] of var 1..3: values;
var 1..3: v;

predicate not_in(var int: v, array[int] of var int: values) =
  forall(i in index_set(values)) (
     values[i] != v
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(values, [1,3]) /\
  v = 2 /\
  not_in(v, values)
;

output
[
  "values: " ++ show(values) ++ "\n" ++
  "v: " ++ show(v) ++ "\n" 
];
