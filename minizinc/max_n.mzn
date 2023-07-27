% 
% Global constraint max_n in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmax_n.html
% """
% Constraint
%
%     max_n​(MAX,​RANK,​VARIABLES)​
%
% Purpose
%
%     MAX is the maximum value of rank RANK (i.e., the RANKth largest 
%     distinct value) of the collection of domain variables VARIABLES. 
%     Sources have a rank of 0.
%
% Example
%     ​(6, 1, <3, 1, 7, 1, 6>)
%
%     The max_n constraint holds since its first argument MAX=6 is 
%     fixed to the second (i.e., RANK+1) largest distinct value 
%     of the collection <3, 1, 7, 1, 6>.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..7: variables;
var 1..7: t_max;

% rank start from 0, i.e. counts how many values 
% are larger than t_max
var 0..n-1: rank; 

solve satisfy;

%
% This is probably way too complicated, but in some way we must
% handle duplicate values.
%
predicate max_n(var int: t_max, 
                var int: rank, 
                array[int] of var int: variables) =
  let {
      int: li = min(index_set(variables)),
      int: ui = max(index_set(variables)),
      array[li..ui] of var 0..1: first_pos
  } in

   % t_max must be in variables (needed for multidirection) 
   exists(i in index_set(variables)) ( t_max = variables[i]  )

   /\ % set 1 in first_pos where variables[i] is the first
      % occurrence of this value.
   forall(i in index_set(variables)) (
      exists(j in li..i-1) (
        variables[j] = variables[i]
      ) <->  first_pos[i] = 0
   )
   /\ % how many elements are larger than t_max?
   rank = sum(i in index_set(first_pos)) (
     bool2int(first_pos[i] = 1 /\ variables[i] > t_max)
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
   cp1d(variables, [3,1,7,1,6])
   /\
   % t_max = 6
   t_max = 1
   % /\
   % rank = 1
   /\
   max_n(t_max, rank, variables)

;

output [
  "variables: " ++ show(variables) ++ "\n" ++ 
  "t_max: " ++ show(t_max) ++ "\n" ++
  "rank: " ++ show(rank) ++ "\n"
];

