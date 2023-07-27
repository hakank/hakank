% 
% Global constraint min in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmin_n.html
% """
% Constraint
%
%     min_n​(MIN,​RANK,​VARIABLES)​
% 
% Purpose
%
%     MIN is the minimum value of rank RANK (i.e., the RANKth 
%     smallest distinct value) of the collection of domain variables 
%     VARIABLES. Sources have a rank of 0.
%
% Example
%     ​(3, 1, <3, 1, 7, 1, 6>)
%
%     The min_n constraint holds since its first argument MIN=3 is 
%     fixed to the second (i.e., RANK+1) smallest distinct value of 
%     the collection <3, 1, 7, 1, 6>. Observe that identical values 
%     are only counted once: this is why the minimum of order 1 is 
%     3 instead of 1.
%
% """
%
% Compare with http://www.hakank.org/minizinc/max_n.mzn
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..7: variables;
var 1..7: t_min;
% rank start from 0, i.e. counts how many values 
% are larger than t_max
var 0..n-1: rank; 

solve satisfy;

%
% This is probably way too complicated, but in some way we must
% handle duplicate values.
%
predicate min_n(var int: t_max, 
                var int: rank, 
                array[int] of var int: variables) =
  let {
      int: li = min(index_set(variables)),
      int: ui = max(index_set(variables)),
      array[li..ui] of var 0..1: first_pos
  } in

   % t_min must be in variables (needed for multidirection) 
   exists(i in index_set(variables)) ( t_min = variables[i]  )

   /\ % set 1 in first_pos where variables[i] is the first
      % occurrence of this value.
   forall(i in index_set(variables)) (
      exists(j in li..i-1) (
        variables[j] = variables[i]
      ) <->  first_pos[i] = 0
   )
   /\ % how many elements are larger than t_max?
   rank = sum(i in index_set(first_pos)) (
     bool2int(first_pos[i] = 1 /\ variables[i] < t_min)
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
   % cp1d(variables, [3,1,7,1,6])
   % /\
   t_min = 3
   /\
   rank = 1
   /\
   min_n(t_min, rank, variables)

;

output [
  "variables: " ++ show(variables) ++ "\n" ++ 
  "t_min: " ++ show(t_min) ++ "\n" ++
  "rank: " ++ show(rank) ++ "\n"
];
