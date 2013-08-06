% 
% Dominating Set Problem in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Dominating_set_problem
% """
% An instance of the dominating set problem consists of:
%
%    * a graph G with a set V of vertices and a set E of edges, and
%    * a positive integer K smaller than or equal to the number of vertices in G.
%
% The problem is to determine whether there is a dominating set of size K or less for G. 
% That is, we want to know if there is a subset D of V of size less than or equal to K 
% such that every vertex in V-D is joined to at least one member of D by an edge in E.
% 
% In the graph [below], the set {4,5} is an example of a dominating set of size two.
%
%      6 
%       \
%        4 -- 5
%        |    | \ 
%        |    |  1
%        |    | /
%        3 -- 2 
%
% """
 
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_nodes;
int: num_edges;
array[1..num_edges, 1..2] of 1..num_nodes: graph; % assumes an undirected graph
var set of 1..num_nodes: D; % the dominating set
0..num_nodes: K; % limit of the cardinality of D

%
% dominating_set(graph, D, K, num_nodes)
%
predicate dominating_set(array[int, 1..2] of int: g, var set of int: dom_set, int: k, set of int: nodes) =
   % for all nodes, there are at least one connection to a member in D 
   forall(j in nodes) (
     exists(e in nodes, i in 1..card(index_set_1of2(g))) (
        e in dom_set
        /\
        j != e  
        /\
        (
          (g[i,1] = j /\ g[i,2] = e)
          \/
          (g[i,1] = e /\ g[i,2] = j)
        )
     )
   )
   /\
   card(dom_set) <= k
;

% solve minimize card(D);
% solve satisfy;
solve :: set_search([D], input_order, indomain_min, complete) satisfy;

constraint
   dominating_set(graph, D, K, 1..num_nodes)

;

output [
  "D: ", show(D), "\n",
];

%
% data
%

% The first example from http://en.wikipedia.org/wiki/Dominating_set_problem
% num_nodes = 6;
% num_edges = 8;
% K = 2;
% % Note: assumes undirected graph
% graph = array2d(1..num_edges, 1..2, [
%   1,2,
%   1,5,
%   2,1,
%   2,3,
%   2,5,
%   3,4,
%   4,5,
%   4,6,
% ]);


% Second example from http://en.wikipedia.org/wiki/Dominating_set_problem
num_nodes = 8;
num_edges = 11;
K = 3; % 3 nodes in D is enough: {2,4,8} or {2,6,8}
graph = array2d(1..num_edges, 1..2, [
  1,2,
  1,8,
  2,3,
  2,4,
  2,8,
  3,4, 
  4,5,
  5,6,
  6,7,
  6,8,
  7,8
]);

