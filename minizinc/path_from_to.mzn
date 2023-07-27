% 
% Global constraint path_from_to in MiniZinc.
%
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cpath_from_to.html
% """
% Select some arcs of a digraph G so that there is still a path 
% between two given vertices of G.
% 
% Example
%  (
%  4, 3, <
%  index-1 succ-{},
%  index-2 succ-{},
%  index-3 succ-{5},
%  index-4 succ-{5},
%  index-5 succ-{2, 3}
%  >
% )
% 
% The path_from_to constraint holds since within the digraph G corresponding 
% to the item of the NODES collection there is a path from vertex FROM=4 
% to vertex TO=3: this path starts from vertex 4, enters vertex 5, and 
% ends up in vertex 3.
%
% """

 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: num_nodes;
array[1..num_nodes, 1..num_nodes] of var 0..1: g;
int: len;
array[1..len] of var 1..num_nodes: paths;
var int: from;
var int: to;


%
% path(LEN, GRAPH, PATHS)
%
predicate path(array[int,int] of var int: g, array[int] of var int: paths) = 
   let  {
     int: len = length(paths)
   }  
   in
   forall(i in 2..len) (
     g[paths[i-1], paths[i]] > 0
   )
;

%
% path_from_to(FROM, TO, GRAPH, PATHS)
%
% Note: The fourth parameter PATHS is an array of the path, and
% it must have a known length (since MiniZinc cannot handle arrays with 
% dynamic lengths).
%
% (There is no guarantee that the path given is the shortest one.)
%
predicate path_from_to(var int: from, var int: to, array[int,int] of var int: g, array[int] of var int: paths) =
     let {
        int: lbx = min(index_set(paths)),
        int: ubx = max(index_set(paths))
     }
     in
     path(g, paths)
     /\
     paths[lbx] = from
     /\
     exists(j in index_set(paths)) (
       paths[j] = to
       /\
       not exists(k in index_set(paths) where j != k) ( 
           paths[k] = to
       )
     )
;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

solve satisfy;

constraint
  % The graph from Global Constraint Catalogue:
  cp2d(g, 
   [|0,0,0,0,0, % 1
    |0,0,0,0,0, % 2
    |0,0,0,0,1, % 3
    |0,0,0,0,1, % 4
    |0,1,1,0,0|])
%  cp2d(g,
%   [|0,1,0,1,0, % 1
%    |1,0,1,0,1, % 2
%    |0,1,0,0,1, % 3
%    |1,0,0,0,1, % 4
%    |0,1,1,1,0|])
%
   /\
   path_from_to(from,to, g, paths)
   /\
   from = 4
   /\
   to = 3
;

output
[ 
  if j = 1 then "\n" else " " endif ++
    show(g[i, j])
  | i,j in 1..num_nodes
]
++ 
["\n"]
++
 [
  show(paths), "\n"
] 
; 

%
% data
%
len = 5;
num_nodes = 5;
