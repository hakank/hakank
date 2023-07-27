% 
% Global constraint max_size_set_of_consecutive_var in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmax_size_of_consecutive_var.html
% """
% Constraint
%
%     max_size_set_of_consecutive_var​(MAX,​VARIABLES)​
%
% Purpose
%
%     MAX is the size of the largest set of variables of the 
% collection VARIABLES that all take their value in a set of consecutive values.
%
% Example
%     (
%     6,​<
%     var-3,
%     var-1,
%     var-3,
%     var-7,
%     var-4,
%     var-1,
%     var-2,
%     var-8,
%     var-7,
%     var-6
%     >
%     )
%
%     In the example, the two sets {3, 1, 3, 4, 1, 2} and​{7, 8, 7, 6}  
%     take respectively their values in the two following sets of 
%     consecutive values ​{1, 2, 3, 4}​ and ​{6, 7, 8}. Consequently, 
%     the max_size_set_of_consecutive_var constraint holds since the 
%     cardinality of the largest set of variables is 6.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 10;
array[1..n] of var 1..9: variables;
var 1..n: max_set;
solve satisfy;


predicate max_size_set_of_consecutive_var(var int: max_set, 
                                          array[int] of var int: variables) =
    let {
        int: lbv = min(index_set(variables)),
        int: ubv = max(index_set(variables)),
        int: low = lb_array(variables),
        int: up  = ub_array(variables),
        array[low..up] of var 0..ubv: occ,
        array[low..up, low..up] of var 0..ubv: mat
    } in

    global_cardinality(variables, set2array(index_set(occ)), occ)
    /\
    forall(i,j in index_set(occ)) (
       % mat[i,j] is the sum of all element in occ[i..j], but
       % only if they all are > 0. If some value in the range
       % is 0, then mat[i,j] is 0
       mat[i,j] = bool2int(forall(k in i..j) ( occ[k] > 0 ))*sum(k in i..j) (occ[k] )
    )
    /\
    maximum(max_set, [mat[i,j] | i,j in low..up])

;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
    max_set = 6
    /\
    % cp1d(variables, [3,1,3,7,4,1,2,8,7,6])
    % cp1d(variables,   [3,1,3,7,4,1,2,8,4,9])
    % /\
    max_size_set_of_consecutive_var(max_set, variables)
;

output
[
  "variables:" ++ show(variables) ++ "\n" ++
  "max_set:" ++ show(max_set) ++ "\n"
];
