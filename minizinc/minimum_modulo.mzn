% 
% Global constraint minimum_modulo in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cminimum_modulo.html
% """
% Constraint
%
%     minimum_modulo​(MIN,​VARIABLES,​M)​

% Purpose
%
%     MIN is a minimum value of the collection of domain variables 
%     VARIABLES according to the following partial ordering: 
%     (X mod M) < (Y mod M).
%
% Example
%     ​(6,​<9, 1, 7, 6, 5>, 3)
%     ​(9,​<9, 1, 7, 6, 5>, 3)
%
%     The minimum_modulo constraints hold since MIN is respectively 
%     set to values 6 and 9, where 6 mod 3=0 and 9 mod 3=0 are both 
%     less than or equal to all the expressions 
%     9 mod 3=0, 1 mod 3=1, 7 mod 3=1, 6 mod 3=0, and 5 mod 3=2.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..9: variables;
var 1..9: t_min;
var 0..9: m;

predicate minimum_modulo(var int: t_min, 
                         array[int] of var int: variables,
                         var int: m) =
    let {
       int: lbv = min(index_set(variables)),
       int: ubv = max(index_set(variables)),
       int: u = ub_array(variables),
       array[lbv..ubv] of var 0..u: mod_array,
       var 0..u: mod_min
   } in
   forall(i in index_set(variables)) (
      % mod_array[i] = variables[i] `mod` m 
      mod_array[i] = variables[i] mod m 
   )
   /\
   minimum(mod_min, mod_array)
   /\
   exists(i in index_set(mod_array)) (
        mod_array[i] = mod_min /\
        t_min = variables[i]
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
   t_min = 6 /\
   % t_min = 9 /\
   cp1d(variables, [9,1,7,6,5])
   /\
   m = 3
   /\
   minimum_modulo(t_min, variables, m)
;

output
[
 "variables: " ++ show(variables) ++ "\n" ++
 "t_min: " ++ show(t_min) ++ "\n" ++
 "m: " ++ show(m) ++ "\n"


];

