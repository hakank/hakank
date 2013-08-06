% 
% Graph partition in MiniZinc.
% 
% From Pascal Van Hentenryck & Laurent Michel:
%  "Constraint-based local search", page 6f.
% where the object is to partition a graph such that
%  - the number of nodes in each partition is the same
%  - the number of edges between each partition is minimized (the cost).
% 
% The graph is from page 6 (the connections are in the array nodes below).
% 
% There are 4 solution with minimal number of edges between 
% the two sets (5). Node 1 has been set to partition 1 (symmetry breaking).
% The first solution is the one in the book, as well as the one Gecode/fz gives
% when minimizing edges_between.
%
%   {1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 24}, {6, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23}
%
% Here are the other three solutions:
%   {1, 3, 4, 5, 7, 8, 9, 10, 11, 12, 20, 24}, {2, 6, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23}
%   {1, 4, 5, 7, 8, 9, 10, 11, 12, 20, 22, 24}, {2, 3, 6, 13, 14, 15, 16, 17, 18, 19, 21, 23}
%   {1, 4, 5, 8, 9, 10, 11, 12, 20, 22, 23, 24}, {2, 3, 6, 7, 13, 14, 15, 16, 17, 18, 19, 21}
%
% For three partitions (m=3), this model yields one solution with cost 7:
%     {1, 2, 3, 4, 5, 7, 8, 9}, {10, 11, 12, 20, 21, 22, 23, 24}, {6, 13, 14, 15, 16, 17, 18, 19}
% 
% 
% For four partitions (m=4) the minimal cost is 10, e.g. the following solution (which is
% unique apart from symmetries)
%   {1, 3, 4, 5, 7, 8}, {6, 13, 14, 15, 16, 17}, {9, 10, 11, 12, 20, 24}, {2, 18, 19, 21, 22, 23}
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 24; % number of nodes
int: m = 2; % number of partitions (2,3,4,6,12 i.e. the divisors of n)
array[1..n] of set of 1..n: nodes;


% the partitions
array[1..m] of var set of 1..n: x;

% number of edges between the two partitions
var 0..n*n: edges_between;


% solve satisfy;
% solve minimize edges_between;
% solve :: set_search(x, largest, indomain_min, complete) satisfy;
solve :: set_search(x, largest, indomain_min, complete) minimize edges_between;


constraint
   assert(n mod m = 0, "m must be a multiple of n")
;


% partition the nodes
constraint
   partition_set(x, 1..n)
   /\ 
   forall(i in 2..m) (
      card(x[i-1]) = card(x[i])
   )
;

%
% cost is the number of edges between the m sets
%
constraint
   edges_between = sum(i in 1..n) (
                      % bool2int(i in x[1])*sum(j in nodes[i]) (bool2int(j in x[2]) )
                      % % sum(j in nodes[i]) ( bool2int( (i in x[1] /\ j in x[2]) ))

                      % a more general version to allow for more than 2 partitions
                      sum(p1, p2 in 1..m where p1 < p2) (
                             bool2int(i in x[p1])*sum(j in nodes[i]) (bool2int(j in x[p2]) )
                      )
                   )

   % /\ edges_between <= 5 % for m=2
   % /\ edges_between <= 7 % for m=3 
   % /\ edges_between <= 10 % for m=4

   % /\ increasing(x) %  Don't work for all solvers (e.g. Gecode/fz)

   % /\ % Test of a a poor man's increasing for Gecode/fz etc 
   %    % that don't support "<" for sets.
   %    % Don't work though...
   % forall(i in 2..m) (
   %    min(set2array(lb(x[i-1]))) < min(set2array(lb(x[i])))
   % )

;



% symmetry breaking:
% put node 1 is in first partition
constraint
  1 in x[1]
;

output [
  "edges_between:" ++ show(edges_between) ++ "\n"
] ++
[
    show(x[i]) ++ "\n"
  | i in 1..m
]
 ++ ["\n"]
;


%
% data
%
nodes = [
{3,4},                % 1
{3,6},                % 2
{1,2},                % 3
{1,5,12},             % 4
{4,7,8},              % 5
{3,7,13,14,15,16,17}, % 6
{5,6,9},              % 7
{5,9},                % 8
{7,8,10,12},          % 9
{9,11,20},            % 10
{10,12,20,24},        % 11
{4,9,11,24},          % 12
{6,14},               % 13
{6,16,18},            % 14
{6,16},               % 15
{6,15,17,18},         % 16
{6,16,20},            % 17
{14,16,19,20,21},     % 18
{18,23},              % 19
{10,11,17,22},        % 20
{18,23},              % 21
{20,23},              % 22
{21,22,24},           % 23
{11,12,23},           % 24
];