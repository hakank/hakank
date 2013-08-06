% 
% Generalized Assignment Problem in MiniZinc.
% 
% From GLPK:s example gap.mod
% """
% GAP, Generalized Assignment Problem
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
% 
% The Generalized Assignment Problem (GAP) is to assign a set of jobs
% to a set of agents subject to the constraints that each job must be
% assigned exactly to one agent and the total resources consumed by all
% jobs assigned to an agent must not exceed the agent's capacity.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% number of agents 
int: m;

% number of jobs 
int: n;

% set of agents 
set of int: I = 1..m;

% set of jobs 
set of int: J = 1..n;

% resource consumed in allocating job j to agent i 
array[I,J] of int: a; 

% resource capacity of agent i 
array[I] of int: b;

% cost of allocating job j to agent i 
array[I,J] of int: c;

% x[i,j] = 1 means job j is assigned to agent i 
array[I,J] of var 0..1: x;


% the objective is to find cheapest assignment (note that gap can also
% be formulated as maximization problem) 
var int: obj = sum(i in I, j in J) (c[i,j] * x[i,j]);
solve :: int_search( 
        [x[i,j] | i in I, j in J] ++ [obj] , 
        first_fail, indomain_min, complete)
    minimize obj;
    % maximize obj;

constraint
  obj >= 0
  /\

  % job j must be assigned exactly to one agent 
  forall(j in J) (sum(i in I) (x[i,j]) = 1)

  /\
  % total amount of resources consumed by all jobs assigned to agent i
  %  must not exceed the agent's capacity 
  forall(i in I) ( sum(j in J) (a[i,j] * x[i,j]) <= b[i])

;

output [
  "\nobj: ", show(obj), 
 
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j]) 
  | i in I, j in J
] ++ ["\n"];

%
% data
% 

% """
% These data correspond to the instance c515-1 (gap1) from:
%
%   I.H. Osman, "Heuristics for the Generalised Assignment Problem:
%   Simulated Annealing and Tabu Search Approaches", OR Spektrum, Volume
%   17, 211-225, 1995
%
%   D. Cattrysse, M. Salomon and L.N. Van Wassenhove, "A set partitioning
%   heuristic for the generalized assignment problem", European Journal
%   of Operational Research, Volume 72, 167-174, 1994 
%
% The optimal solution is 261 (minimization) or 336 (maximization) 
% """

m = 5;

n = 15;

a = array2d(I,J, [
 8,15,14,23, 8,16, 8,25, 9,17,25,15,10, 8,24,
15, 7,23,22,11,11,12,10,17,16, 7,16,10,18,22,
21,20, 6,22,24,10,24, 9,21,14,11,14,11,19,16,
20,11, 8,14, 9, 5, 6,19,19, 7, 6, 6,13, 9,18,
 8,13,13,13,10,20,25,16,16,17,10,10, 5,12,23]);

b = [36,34,38,27,33];

c = array2d(I, J,[
17,21,22,18,24,15,20,18,19,18,16,22,24,24,16,
23,16,21,16,17,16,19,25,18,21,17,15,25,17,24,
16,20,16,25,24,16,17,19,19,18,20,16,17,21,24,
19,19,22,22,20,16,19,17,21,19,25,23,25,25,25,
18,19,15,15,21,25,16,16,23,15,22,17,19,22,24]);


