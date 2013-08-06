% 
% Maximum Cut Problem in MiniZinc.
% 
% From GLPK:s model max_cut.mod
% """
% MAXCUT, Maximum Cut Problem
% 
% Written in GNU MathProg by Andrew Makhorin <mao@gnu.org>
%
% The Maximum Cut Problem in a network G = (V, E), where V is a set
% of nodes, E is a set of edges, is to find the partition of V into
% disjoint sets V1 and V2, which maximizes the sum of edge weights
% w(e), where edge e has one endpoint in V1 and other endpoint in V2.
%
% Reference:
% Garey, M.R., and Johnson, D.S. (1979), Computers and Intractability:
% A guide to the theory of NP-completeness [Network design, Cuts and
% Connectivity, Maximum Cut, ND16].
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_edges;
int: num_nodes;

% w[i,j] is weight of edge (i,j) 
array[1..num_edges] of 0..1: w; 


% """
% x[i] = 0 means that node i is in set V1
% x[i] = 1 means that node i is in set V2 
%
% 
%  We need to include in the objective function only that edges (i,j)
%    from E, for which x[i] != x[j]. This can be modeled through binary
%    variables s[i,j] as follows:
%
%       s[i,j] = x[i] xor x[j] = (x[i] + x[j]) mod 2,                  (1)
%
%    where s[i,j] = 1 iff x[i] != x[j], that leads to the following
%    objective function:
%
%       z = sum{(i,j) in E} w[i,j] * s[i,j].                           (2)
%
%    To describe "exclusive or" (1) we could think that s[i,j] is a minor
%    bit of the sum x[i] + x[j]. Then introducing binary variables t[i,j],
%    which represent a major bit of the sum x[i] + x[j], we can write:
%
%       x[i] + x[j] = s[i,j] + 2 * t[i,j].                             (3)
%
%    An easy check shows that conditions (1) and (3) are equivalent.
%
%    Note that condition (3) can be simplified by eliminating variables
%    s[i,j]. Indeed, from (3) it follows that:
%
%       s[i,j] = x[i] + x[j] - 2 * t[i,j].                             (4)
%
%    Since the expression in the right-hand side of (4) is integral, this
%    condition can be rewritten in the equivalent form:
%
%       0 <= x[i] + x[j] - 2 * t[i,j] <= 1.                            (5)
%
%    (One might note that (5) means t[i,j] = x[i] and x[j].)
%
%    Substituting s[i,j] from (4) to (2) leads to the following objective
%    function:
%
%       z = sum{(i,j) in E} w[i,j] * (x[i] + x[j] - 2 * t[i,j]),       (6)
%
%    which does not include variables s[i,j]. 
% """


% Note: GLPK:s solution

% x[1]    1
% x[2]    0
% x[3]    1
% x[4]    0
% x[5]    0
% x[6]    1
% x[7]    1
% x[8]    0
% x[9]    1
% x[10]   1
% x[11]   0
% x[12]   0
% x[13]   1
% x[14]   0
% x[15]   1
% i.e.
% 1,0,1,0,0,1,1,0,1,1,0,0,1,0,1 % GLPK
% 1,0,1,0,0,1,1,0,1,0,1,1,0,1,0 % ECLiPSe/ic
% 0,1,0,1,1,0,0,1,0,1,0,0,1,0,1 % Minizinc/mip
% 1,0,1,0,0,1,1,0,1,0,1,1,0,1,0 % Gecode/fz
% 1,0,1,0,0,1,1,0,1,0,1,1,0,1,0 % Minizinc/flatzinc
% 1,0,1,0,0,1,1,0,1,0,1,1,0,1,0 % Minizinc/fdmip
% 0,1,0,1,1,0,0,1,0,0,1,1,0,1,0
% 0,1,0,1,1,0,0,1,0,0,1,1,0,1,0

array[1..num_nodes] of var 0..1: x;

array[1..num_edges, 1..2] of 1..num_nodes: E;

% t[i,j] = x[i] and x[j] = (x[i] + x[j]) div 2 
array[1..num_edges, 1..num_nodes] of var 0..1: t;


% see (6)
var int: z;
solve :: int_search(
    [x[i] | i in 1..num_nodes] ++
    [t[i,j] | i in 1..num_edges, j in 1..num_nodes] ++
    [z],
    first_fail, % "occurrence",
    indomain_min,
    complete
    ) 
    % satisfy;
    maximize z;


constraint
   z >= 0
   /\
  
   % see (4) 
   forall(i in 1..num_edges) (
      0 <= x[E[i,1]] + x[E[i,2]] - 2 * t[E[i,1],E[i,2]] 
      /\
      x[E[i,1]] + x[E[i,2]] - 2 * t[E[i,1],E[i,2]] <= 1
   )
   /\ 
   z = sum(i in 1..num_edges) (
         w[i] * (x[E[i,1]] + x[E[i,2]] - 2 * t[E[i,1],E[i,2]])
       )

   % /\ 
   % z >= 20
   % /\
   % cp1d(x, [0,1,0,1,1,0,0,1,0,0,1,1,0,1,0])
   % cp1d(x, [1,0,1,0,0,1,1,0,1,1,0,0,1,0,1])
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 



%
% data
% 

% """
% In this example the network has 15 nodes and 22 edges.
%
% Optimal solution is 20
% """
num_edges = 22;
num_nodes = 15;
E = array2d(1..num_edges, 1..2, [
   1, 2, 
   1, 5, 
   2, 3, 
   2, 6, 
   3, 4, 
   3, 8, 
   4, 9, 
   5, 6, 
   5, 7, 
   6, 8, 
   7, 8, 
   7, 12, 
   8, 9, 
   8, 12, 
   9, 10, 
   9, 14, 
   10, 11, 
   10, 14, 
   11, 15, 
   12, 13, 
   13, 14, 
   14, 15]);

% weights are just 1
w = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1];


output
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];
