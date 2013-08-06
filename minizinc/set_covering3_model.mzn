%
% Set covering in MiniZinc.
%
% This is just the model, see set_covering3.mod for data.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_senators;
int: num_groups; 
% array[1..num_senators] of int: senators;
% array[1..num_groups] of int: group;

% decision variable: should this senator belong to the committe
array[1..num_senators] of var 0..1: x;
array[1..num_groups, 1..num_senators] of 0..1:  belongs; % matrix for which group a senator belongs

var int: numNeeded; 

%
%  set_covering3
%      mat: an incidence (1/0) matrix of n_group x n_senators, 1 is a senator
%         belongs to the group, else 0
%      assign: the array of assignment of senators,  1 if this senator is assigned, else 0
%      n_groups: number of groups
%      n_senators: number of senators 
%      numNeeded: (result) number of senators needed
%
predicate set_covering3(array[int, int] of int: mat, array[int] of var int: assign, int: n_groups, int: n_senators, var int: numNeeded) =
           forall(i in 1..n_groups) (
              sum(j in 1..n_senators) (assign[j]*mat[i,j]) >= 1
           ) /\ 
           numNeeded = sum(i in 1..num_senators) (x[i])
;

% solve minimize sum(i in 1..num_senators) (x[i]);
solve satisfy;

constraint sum(i in 1..num_senators) (x[i]) = 2;


constraint 
  set_covering3(belongs, x, num_groups, num_senators, numNeeded)
;

