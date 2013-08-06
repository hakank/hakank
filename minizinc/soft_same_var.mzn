% 
% Global constraint soft_same_var in MiniZinc.
% 
% From Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/Csoft_same_var.html
% """
% soft_same_var(C, VARIABLES1, VARIABLES2)
%
% Purpose
%
%  C is the minimum number of values to change in the VARIABLES1 and VARIABLES2 collections 
% so that the variables of the VARIABLES2 collection correspond to the variables of the 
% VARIABLES1 collection according to a permutation.
% 
% Example
%     (
%     4, <9, 9, 9, 9, 9, 1>,
%     <9, 1, 1, 1, 1, 8>
%     )
%
% As illustrated by Figure 4.264.1 [see the URL above], there is a correspondence between two 
% pairs of values of the collections <9, 9, 9, 9, 9, 1> and <9, 1, 1, 1, 1, 8>. Consequently, we 
% must unset at least 6-2 items (6 is the number of items of the VARIABLES1 and VARIABLES2 
% collections). The soft_same_var constraint holds since its first argument C is set to 6-2.
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 6;
array[1..n] of var 1..9: variables1;
array[1..n] of var 1..9: variables2;
var 0..n: c;

predicate soft_same_var(var int: c, array[int] of var int: variables1,
                                    array[int] of var int: variables2) =
    let {
      array[lb_array(variables1)..ub_array(variables1)] of var 0..ub_array(variables1): gcc1,
      array[lb_array(variables2)..ub_array(variables2)] of var 0..ub_array(variables2): gcc2% ,
      % array[lb_array(variables2)..ub_array(variables2)] of var 0..length(variables1): gcc_diff % for the variant
    } 
    in
    assert(length(variables1) = length(variables2), 
           "length of variables1 and variables2 must be the same")
    /\
    global_cardinality(variables1, set2array(index_set(gcc1)), gcc1) 
    /\
    global_cardinality(variables2, set2array(index_set(gcc2)), gcc2) 
    
    /\ % count the total number of differences divided by 2
    c = sum(i in index_set(gcc1)) (
      abs(gcc1[i]-gcc2[i])
    ) div 2

    % variant: count the number of entries where gcc1 are larger than gcc2
    %          requires the extra temporary array gcc_diff.
    % forall(i in index_set(gcc_diff)) (
    %     (gcc1[i] > gcc2[i] -> gcc_diff[i] = gcc1[i] - gcc2[i])
    %     /\
    %     (gcc1[i] <= gcc2[i] -> gcc_diff[i] = 0)
    % )
    % /\
    % c = sum(gcc_diff)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;
% solve :: int_search(variables1 ++ variables2 ++ [c], "first_fail", "indomain", "complete") satisfy;

constraint
   cp1d(variables1, [9,9,9,9,9,1])
   /\
   cp1d(variables2, [9,1,1,1,1,8])
   /\
   soft_same_var(c, variables1, variables2)
   /\
   c = 4
   % c = 1
;

output
[
  "variables1: " ++ show(variables1) ++ "\n" ++
  "variables2: " ++ show(variables2) ++ "\n" ++
  "c: " ++ show(c) ++ "\n" 
];
