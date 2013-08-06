%
% Max flow problem in Minzinc
%
% From Winston "Operations Research", page 420f, 423f
% Sunco Oil example.

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of 1..5: nodes = 1..5;

int: num_arcs = 7;
array[1..num_arcs, 1..2] of int: arcs;
array[1..num_arcs] of int: cap;

array[nodes, nodes] of var int: flow;

var int: z = flow[5,1];
solve maximize z;

constraint

   forall(i,j in nodes) (
      flow[i,j] >= 0
   )
   /\
   forall(i in 1..num_arcs) (
      flow[arcs[i,1], arcs[i,2]] <= cap[i]
   )
   /\
   forall(i in nodes) (
      sum(k in 1..num_arcs where arcs[k,2] = i ) (
           flow[arcs[k,1], arcs[k,2]]
           
      ) = sum(k in 1..num_arcs where arcs[k,1] = i) (
        flow[arcs[k,1], arcs[k,2]] 
      )
   )
;


cap = [2,3,3,4,2,1,100];
arcs = array2d(1..num_arcs, 1..2,
  [1, 2,
   1, 3,
   2, 3,
   2, 4,
   3, 5,
   4, 5,
   5, 1
])
;

output [ "z: ", show(z), ] ++
[
  if j = 1 then  "\n" else " " endif ++
  show(flow[i,j])
 | i in nodes, j in nodes
];

