% 
% Assignment problem in MiniZinc.
% 
% From GLPK:s example assign.mod:
% """
% The assignment problem is one of the fundamental combinatorial
% optimization problems.
%
% In its most general form, the problem is as follows:
%
% There are a number of agents and a number of tasks. Any agent can be
% assigned to perform any task, incurring some cost that may vary
% depending on the agent-task assignment. It is required to perform all
% tasks by assigning exactly one agent to each task in such a way that
% the total cost of the assignment is minimized.
%
% (From Wikipedia, the free encyclopedia.) 
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% I added (an commented away) an assigned array for better output
% than showing the assignment matrix x.
% 
% As of 2008-12-14 (ROTD version of MiniZinc):
%
% Gecode/fz is fast when not using the assigned array.
% 
% MiniZinc/flatzinc gives the following error:
%    evaluation of model would result in integer overflow.
% 
% The IP-solvers and ECLiPSe/ic are fast.
%


% number of agents 
int: m;

% number of tasks
int: n;

% set of agents 
set of int: I = 1..m;

% set of tasks 
set of int: J = 1..n;

% cost of allocating task j to agent i 
array[I,J] of int: c;

% For the output in MiniZinc: the assignment as task number.
% This makes the constraint solvers work harder, though the IP solvers 
% has no problems.
array[I] of var J: assigned; 
int: cmax = max([c[i,j] | i in I, j in J]);
array[I] of var 0..cmax: costs; % the cost per assignment

% x[i,j] = 1 means task j is assigned to agent i
% note that variables x[i,j] are binary, however, there is no need to
% declare them so due to the totally unimodular constraint matrix 
array[I,J] of var 0..1: x;

var 0..cmax*n: z;

% the objective is to find a cheapest assignment 
% solve :: int_search(
%         assigned ++ 
%         costs ++
%         [x[i,j] | i in I, j in J] ++
%         [c[i,j] | i in I, j in J],
%         "first_fail", "indomain_min", "complete") minimize z;
solve minimize z;

constraint
  forall(i in I, j in J) (
     c[i,j] >= 0
     /\
     x[i,j] >= 0
  )
  /\
  % each agent can perform at most one task 
  forall(i in I) (sum(j in J) (x[i,j]) <= 1)
  /\
  % each task must be assigned exactly to one agent 
  forall(j in J) (sum(i in I) (x[i,j]) = 1)
  /\
  % the total cost
  z = sum(i in I, j in J) (c[i,j] * x[i,j])
  /\
  % to which task and what cost is person i assigned (for output in MiniZinc)
  forall(i in I) (
    assigned[i] = sum(j in J) (j*x[i,j]) 
    /\   
    costs[i] = sum(j in J) (c[i,j]*x[i,j]) 
  )
;


%
% data;
% 

% """
% These data correspond to an example from [Christofides].
%
% Optimal solution is 76
% """

m = 8;
n = 8;

c = array2d(1..m, 1..n, [
13, 21, 20, 12,  8, 26, 22, 11,
12, 36, 25, 41, 40, 11,  4,  8,
35, 32, 13, 36, 26, 21, 13, 37,
34, 54,  7,  8, 12, 22, 11, 40,
21,  6, 45, 18, 24, 34, 12, 48,
42, 19, 39, 15, 14, 16, 28, 46,
16, 34, 38,  3, 34, 40, 22, 24,
26, 20,  5, 17, 45, 31, 37, 43]);


output [
  "z: ", show(z),"\n",
  "assigned: ", show(assigned), "\n",
  "costs: ", show(costs), "\n",
] ++ 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in I, j in J
]
;
