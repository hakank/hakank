% 
% Global constraint maximum_modulo in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmaximum_modulo.html
% """
% Constraint
%
%     maximum_modulo​(MAX,​VARIABLES,​M)​
% 
% Purpose
%
%     MAX is a maximum value of the collection of domain variables 
%     VARIABLES according to the following partial ordering: ​
%     (X mod M) <​(Y mod M).
%
% Example
%     ​(5, <9, 1, 7, 6, 5>, 3)
%
%     The maximum_modulo constraint holds since its first argument MAX 
%      is set to value 5, where 5 mod 3=2 is greater than or equal to 
%      all the expressions 9 mod 3=0, 1 mod 3=1, 7 mod 3=1 and 6 mod 3=0.

% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..9: variables;
var 1..9: t_max;
var 0..9: m;

predicate maximum_modulo(var int: t_max, 
                         array[int] of var int: variables,
                         var int: m) =
    let {
       int: u = ub_array(variables),
       array[1..n] of var 0..u: mod_array,
       var 0..u: mod_max
   } in
   forall(i in index_set(variables)) (
      mod_array[i] = variables[i] mod m
   )
   /\
   maximum(mod_max, mod_array)
   /\
   exists(i in index_set(mod_array)) (
        mod_array[i] = mod_max /\
        t_max = variables[i]
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
   t_max = 5
   /\
   cp1d(variables, [9,1,7,6,5])
   /\
   m = 3
   /\
   maximum_modulo(t_max, variables, m)
;


output
[
  "variables: " ++ show(variables) ++ "\n" ++ 
  "t_max: " ++ show(t_max) ++ "\n" ++ 
  "m: " ++ show(m) ++ "\n" 
];
