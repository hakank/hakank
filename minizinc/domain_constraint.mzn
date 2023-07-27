% 
% Global constraint domain_constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cdomain_constraint.html
% """
% domain_constraint​(VAR,​VALUES)
% 
% Purpose
%
% Make the link between a domain variable VAR and those 0-1 variables 
% that are associated with each potential value of VAR: The 0-1 variable 
% associated with the value that is taken by variable VAR is equal to 1, 
% while the remaining 0-1 variables are all equal to 0.
%
% Example
%     (
%     5, <
%     var01-0 value-9,
%     var01-1 value-5,
%     var01-0 value-2,
%     var01-0 value-7
%     >
%     )

%  The domain_constraint holds since VAR=5 is set to the value corresponding to the 0-1 variable set to 1, while the other 0-1 variables are all set to 0.

% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
array[1..n] of var 0..1: var01; % or var bool
array[1..n] of var 1..9: values;
var 1..9: val;

predicate domain_constraint(array[int] of var 0..1: var01, array[int] of var int: values) =
  exists(i in index_set(var01)) (
    var01[i] = 1
    /\
    val = values[i]  
  )
  /\
  sum(var01) = 1
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
  cp1d(var01, [0,1,0,0])
  /\
  cp1d(values,[9,5,2,7])
  /\
  val = 5
  /\
  domain_constraint(var01, values)
;



output
[ 
  "var01: " ++ show(var01) ++ "\n" ++
  "values: " ++ show(values) ++ "\n" ++
  "val: " ++ show(val) ++ "\n"

];
