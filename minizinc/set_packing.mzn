%
% Set packing problem in MiniZinc.
%
% Steven Skiena, The Stony Brook Algorithm Repository
% http://www.cs.sunysb.edu/~algorith/files/set-packing.shtml
% """
% Input Description: A set of subsets S = S_1, ..., S_m of the universal set 
% U = {1,...,n}.
% 
% Problem: What is the largest number of mutually disjoint subsets from S?
% """
% 
% The problem instance is the pictures INPUT/OUTPUT.
%

% 
% The answer is
% Set 1,3,5,7
%

include "globals.mzn";

int: num_sets = 7;
int: num_elements = 12;

array[1..num_sets] of var 0..1: x;
var int: numNeeded; 

%%
%% matrix version
%%
array[1..num_sets, 1..num_elements] of 0..1:  belongs = 
  array2d(1..num_sets, 1..num_elements, [
   %  1 2 3 4 5 6 7 8 9 0 1 2  elements
      1,1,0,0,0,0,0,0,0,0,0,0,      % Set 1
      0,1,0,0,0,0,0,1,0,0,0,0,      %     2
      0,0,0,0,1,1,0,0,0,0,0,0,      %     3
      0,0,0,0,0,1,1,0,0,1,1,0,      %     4
      0,0,0,0,0,0,0,0,1,1,0,0,      %     5
      1,1,1,0,1,0,0,0,1,1,1,0,      %     6
      0,0,1,1,0,0,1,1,0,0,1,1,      %     7
]);

%%
%% set approach
%%
% array[1..num_sets] of set of 1..num_elements: belongs =
% [
%  {1,2},             % Set 1
%  {2,8},             %     2
%  {5,6},             %     3
%  {6,7,10,11},       %     4
%  {9,10},            %     5
%  {1,2,3,5,9,10,11}, %     6
%  {3,4,7,8,11,12}    %     7
% ];


%%
%% matrix version
%%
predicate set_packing_matrix(array[int, int] of int: mat, array[int] of var int: assign, int: n_sets, int: n_elements, var int: numNeeded) =
    forall(j in 1..n_elements) (
       sum(i in 1..n_sets) (assign[i]*mat[i,j]) = 1
    ) 
    /\ 
    numNeeded = sum(i in 1..num_sets) (assign[i])
;

%%
%% set version.
%% 
%% Note: This is not very efficient...
%%
predicate set_packing_set(array[int] of set of int: belongs, array[int] of var int: assign, int: n_sets, int: n_elements, var int: n_needed) =
   let {
      array[1..n_sets] of var set of 1..n_elements: ss
   }
   in
   forall(i in 1..n_sets) (
      forall(j in belongs[i]) (
          j in ss[i] <-> assign[i] = 1 
      )
   )
   /\
   partition_set(ss, 1..n_elements)
   /\
   n_elements = sum(i in 1..n_sets) (card(belongs[i])*assign[i])
   /\
   n_needed = sum(assign)
;

% solve satisfy;
solve ::int_search(x, first_fail, indomain_min, complete) satisfy;


constraint 

  set_packing_matrix(belongs, x, num_sets, num_elements, numNeeded) 
%    set_packing_set(belongs, x, num_sets, num_elements, numNeeded)
;

output [
   "numNeeded: ", show(numNeeded), "\n",
   show(x), "\n"
]

