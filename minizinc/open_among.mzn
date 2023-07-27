% 
% Global constraint open_among in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Copen_among.html
% """
% Constraint
%
%     open_among​(S,​NVAR,​VARIABLES,​VALUES)​
%
% Purpose
%
%     Let V be the variables of the collection VARIABLES for which 
%     the corresponding position belongs to the set S. NVAR is the 
%     number of variables of V that take their value in VALUES.

% Example
%     (
%     {2,3,4,5}​,3,
%     <8,5,5,4,1>,
%     <1,5,8>
%     )
%
%     The open_among constraint holds since within the last four 
%     values (i.e., S={2,3,4,5}) of <8,5,5,4,1> exactly 3 
%     values belong to the set of values {1,5,8}.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..8: variables;
var 0..n*2: nvar;
var set of 1..n: s;
% strange: values must either be initialized here or free
var set of 1..8: values = {1,5,8};


predicate open_among(var set of int: s, 
                     var int: nvar, 
                     array[int] of var int: variables,
                     var set of int: values) =

    nvar == sum(i in index_set(variables)) ( 
             bool2int(
                 i in s /\
                 variables[i] in values
             ) 
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
    % it fails if values is defined only here 
    % values =  {1,5,8} /\

    s = {2,3,4,5} /\
    nvar = 3 /\
    cp1d(variables, [8,5,5,4,1]) /\
    open_among(s, nvar, variables, values)
;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "values: " ++ show(values) ++ "\n" ++
  "nvar: " ++ show(nvar) ++ "\n" ++
  "s: " ++ show(s) ++ "\n"
];
