% 
% Global constraint nvalue_common in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnvalue_common.html
% """
% Constraint
%
%     nvalue_on_intersection​(NVAL,​VARIABLES1,​VARIABLES2)​
%
% Purpose
%
%     NVAL is the number of distinct values that both occur in the 
%     VARIABLES1 and VARIABLES2 collections.
%
% Example
%     (
%     2,<1,9,1,5>,​
%     <2,1,9,9,6,9>
%     )
%
%     Observe that the two collections <1,9,1,5> and <2,1,9,9,6,9> 
%     share two values in common (i.e., values 1 and 9). Consequently 
%     the nvalue_on_intersection constraint holds since its first 
%     argument NVAL is set to 2.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
int: m = 6;
array[1..n] of var 1..9: variables1;
array[1..m] of var 1..9: variables2;
var 1..min(m,n): nval;



%
% helper predicate to convert an array to a set
%
predicate array2set(array[int] of var int: x, var set of int: s) =
   forall(i in lb_array(x)..ub_array(x)) (
     (not exists(j in index_set(x)) (x[j] = i ) <-> not (i in s))
   )
;

predicate nvalue_common(var int: nval, 
                        array[int] of var int: v1,
                        array[int] of var int: v2) =
  let {
      var set of lb_array(v1)..ub_array(v2): set1,
      var set of lb_array(v2)..ub_array(v2): set2
  } in                    
  array2set(variables1, set1) /\
  array2set(variables2, set2) /\
  nval = card(set1 intersect set2)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
  cp1d(variables1, [1,9,1,5]) /\
  cp1d(variables2, [2,1,9,9,6,9]) /\
  nval = 2 /\

  nvalue_common(nval, variables1, variables2)

;

output [
"nval: " ++ show(nval) ++ "\n"++
"variables1: " ++ show(variables1) ++ "\n"++
"variables2: " ++ show(variables2) ++ "\n"
];
