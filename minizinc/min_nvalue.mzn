% 
% Global constraint min_nvalue in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmin_nvalue.html
% """
% Constraint
%
%     min_nvalue​(MIN,​VARIABLES)​
%
% Purpose
%
%     MIN is the minimum number of times that the same value is taken 
%     by the variables of the collection VARIABLES.
%
% Example
%     (
%     2,​<
%     var-9,
%     var-1,
%     var-7,
%     var-1,
%     var-1,
%     var-7,
%     var-7,
%     var-7,
%     var-7,
%     var-9
%     >
%     )
%
%     In the example, values 1,​7,​9 are respectively used 3,​5,​2 times. 
%     So the minimum number of time MIN that a same value occurs is 2. 
%     Consequently the min_nvalue constraint holds.
% 
% Usage
%
%     This constraint may be used in order to replace a set of count or 
%     among constraints were one would have to generate explicitly one 
%     constraint for each potential value. Also useful for constraining 
%     the number of occurrences of the less used value without knowing 
%     this value in advance and without giving explicitly a lower limit 
%     on the number of occurrences of each value as it is done in the 
%     global_cardinality constraint.
% """
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..n: variables;
var 1..n: t_min;
solve satisfy;

%
% This is more complicated than max_nvalue 
% (http://www.hakank.org/minizinc/max_nvalue) since the minimum value
% in the occurrence array maybe 0 and must be handled.
%
predicate min_nvalue(var int: tt_min, array[int] of var int: variables) =
    let {
        array[min(index_set(variables))..max(index_set(variables))] of var 0..length(variables): occ
    } in
    % global_cardinality_old(variables, occ)
   global_cardinality(variables, array1d(min(index_set(variables))..max(index_set(variables)), [i | i in min(index_set(variables))..max(index_set(variables))]), occ)
    /\ % some value in occ is larger than 0 and less than any other
       % values (larger than 0)
    forall(i in index_set(occ)) (
      occ[i] > 0 -> t_min <= occ[i]
    )
    /\ % and now we find exactly which value that is
    exists(i in index_set(occ)) (
      t_min = occ[i]
    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint
   % cp1d(variables, [9,1,7,1,1,7,7,7,7,9])
   % /\
   t_min = 2
   /\
   min_nvalue(t_min, variables) 

;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "t_min: " ++ show(t_min) ++ "\n"
];
