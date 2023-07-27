% 
% Global constraint minimum_except_0 in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cminimum_except_0.html
% """
% Constraint
%
%     minimum_except_0​(MIN,​VARIABLES)​
%
% Purpose
%
%     MIN is the minimum value of the collection of domain variables 
%     VARIABLES, ignoring all variables that take 0 as value.
%
% Example
%      (3, <3, 7, 6, 7, 4, 7>)
%      (2, <3, 2, 0, 7, 2, 6>)
%      (1000000, <0, 0, 0, 0, 0, 0>)
%
%     The three examples of the minimum_except_0 constraint 
%     respectively hold since:
%         * Within the first example, MIN is set to the minimum 
%           value 3 of the collection <3, 7, 6, 7, 4, 7>.
%         * Within the second example, MIN is set to the minimum 
%           value 2 (ignoring value 0) of the collection <3, 2, 0, 7, 2, 6>.
%         * Finally within the third example, MIN is set to the 
%           default value 1000000 since all items of the collection 
%           <0, 0, 0, 0, 0, 0> are set to 0.
%
% Remark
%
%     The joker value 0 makes sense only because we restrict the 
%     variables of the VARIABLES collection to take 
%     non-negative values.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 6;
array[1..n] of var 0..7: variables;
var 0..10000: t_min;

solve satisfy;

%
% Note: added the default argument.
%
predicate minimum_except_0(var int: t_min, 
                           array[int] of var int: variables,
                           var int: t_default) =
    
    (
       forall(i in index_set(variables)) (
           variables[i] > 0 <-> t_min <= variables[i]
       )
       /\
       exists(i in index_set(variables)) (
          t_min = variables[i]
       )
    )
    \/
    (
        forall(i in index_set(variables)) (
           variables[i] = 0
        )
        /\
        t_min = t_default
    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint
    % t_min = 3 /\
    % variables = [3, 7, 6, 7, 4, 7] /\

    % t_min = 2 /\
    % cp1d(variables, [3, 2, 0, 7, 2, 6]) /\

    % t_min = 100000 /\
    % cp1d(variables, [0, 0, 0, 0, 0, 0]) /\


    minimum_except_0(t_min, variables, 0)
;

output [
   "variables: " ++ show(variables) ++ "\n" ++
   "t_min    : " ++ show(t_min) 

];
