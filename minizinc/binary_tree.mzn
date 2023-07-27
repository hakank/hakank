% 
% Global constraint binary tree in MiniZinc.
% 
% From Global constraint catalog:
% https://sofdem.github.io/gccat/gccat/Cbinary_tree.html
% """
% Constraint
%
%     binary_tree​(NTREES,​NODES)​
% ...
% Purpose
%
%    Cover the digraph G described by the NODES collection with NTREES 
%    binary trees in such a way that each vertex of G belongs to exactly 
%    one binary tree. The edges of the binary trees are directed from 
%    their leaves to their respective root.
% 
% Example
%     (
%     2,<
%     index-1 succ-1,
%     index-2 succ-3,
%     index-3 succ-5,
%     index-4 succ-7,
%     index-5 succ-1,
%     index-6 succ-1,
%     index-7 succ-7,
%     index-8 succ-5
%     >
%     )
%
%     The binary_tree constraint holds since its second argument corresponds 
%     to the 2 (i.e., the first argument of the binary_tree constraint) 
%     binary trees depicted by Figure 4.38.1. [See the referred page and below]
% """
% 
%
% The graph (two trees):
%       1    7  
%      /\    | 
%     5  6   4
%    / \  
%   8   3
%       |
%       2
%


% Notes:
%
% 1) This model uses a brute force variant by building a 
%    "progress matrix" where each step states the parent
%    node in the tree.
%    The last row contains is all the parents.
%    For the example above the following parent matrix
%    is created:    
%       1 3 5 7 1 3 7 5
%       1 5 1 7 1 5 7 1
%       1 1 1 7 1 1 7 1
%       1 1 1 7 1 1 7 1
%       1 1 1 7 1 1 7 1
%       1 1 1 7 1 1 7 1
%       1 1 1 7 1 1 7 1
%       1 1 1 7 1 1 7 1
%
% 2) It assumes that the successors represents a tree,
%    and don't check for cycles. Cycles will be shown 
%    in the parent matrix as alternating parents. 
%    Maybe this should be detected?
%
%    Come to think of it, it don't even check that it is
%    _binary_ tree(s).
%
% 3) However, it is multidirectional, i.e. it may generate trees
%    given just the value of num_trees.
%
% Note that in the successor array (tree), root(s) has itself as a successor.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 8;
array[1..n] of var 1..n: tree; % successors
var 1..n: num_trees;

% the "parent matrix"
array[1..n,1..n] of var 1..n: parents;

solve :: int_search(
        [parents[i,j] | i,j in 1..n] ++ tree ++ [num_trees],  
        first_fail,
        indomain_min,
        complete)
        satisfy;

%
% binary_tree(num_trees, tree)
%
predicate binary_tree(var int: n_trees, array[int] of var int: tree) =
      let {
         int: lbx = min(index_set(tree)),
         int: ubx = max(index_set(tree)),
         array[lbx..ubx,lbx..ubx] of var lbx..ubx: parents
      } in
      % first row is the tree
      forall(j in lbx..ubx) (
         parents[1,j] = tree[j]
      )
      /\ % now: each step gives the path nearer the root
      forall(i in lbx+1..ubx) (
        forall(j in lbx..ubx) (
           parents[i,j] = tree[parents[i-1,j]]
        )
      )
     /\
     nvalue(num_trees, [parents[n,j] | j in lbx..ubx])
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


constraint

     % cp1d([1,3,5,7,1,3,7,5],tree) % the example from above
     % cp1d([1,1,1,2,2,3,3,4],tree) % as binary as we can come with 8 nodes...
     % cp1d([1,1,1,1,2,5,6,7],tree) % 1 has three nodes, else is down from 2
     % The following has two cycles, 2<->3, 7<->8 which is shown 
     % in the parent matrix as alternating successors.
     % cp1d([1,3,2,4,5,6,8,7],tree) 
     % cp1d([1,2,3,4,5,7,8,6],tree)  % triple cycle 6->7->8
     % /\

     % binary_tree(num_trees, tree)

     
     % hakank: I keep the code here so the parent matrix
     %         can be studied, which is much more fun. :)
     % first row is the tree
     forall(j in index_set(tree)) (
        parents[1,j] = tree[j]
     )
     /\ % now: each step gives the path nearer the root
     forall(i in min(index_set(tree))+1..max(index_set(tree))) (
       forall(j in index_set(tree)) (
          parents[i,j] = tree[parents[i-1,j]]
       )
     )
    /\
    nvalue(num_trees, [parents[n,j] | j in index_set(tree)])
    /\ 
    num_trees = 7
  
;

output[
  "tree           : ", show(tree), "\n",    
  "number of trees: ", show(num_trees), "\n",
] ++
[
  if j = 1 then "\n" else " " endif ++
     show(parents[i,j])
  | i,j in 1..n
] 
++
["\n"];

