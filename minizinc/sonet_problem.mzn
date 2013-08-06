%
% The SONET problem in MiniZinc.
% 
% Translation of the ESSENCE' model in the Minion Translator examples:
% http://www.cs.st-andrews.ac.uk/~andrea/examples/sonet/sonet_problem.eprime
% """
% The SONET problem is a network design problem: set up a network between
% n nodes, where only certain nodes require a connection.
% Nodes are connected by putting them on a ring, where all nodes
% on a ring can communicate. Putting a node on a ring requires a so-called
% ADM, and each ring has a capacity of nodes, i.e. ADMs. There is a certain 
% amount of rings, r, that is available. The objective is to set up a network
% by using a minimal amount of ADMs.
%
%
% About the problem model
%
% The problem model has the amount of rings ('r'), amount of nodes('n'),
% the 'demand' (which nodes require communication) and node-capacity of each 
% ring ('capacity_nodes') as parameters.
% The assignement of nodes to rings is modelled by a 2-dimensional matrix 'rings',
% indexed by the amnount of rings and nodes. The matrix-domain is boolean:
% If the node in column j is assigned to the ring in row i, then rings[i,j] = 1 
% and 0 otherwise. So all the '1's in the matrix 'rings' stand for an ADM.
% Hence the objective is to minimise the sum over all columns and rows of matrix
% 'rings'.
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


%
% The Minion solution is:
% """
% Sol: 0 1 1 0 1
% Sol: 1 0 0 1 0
% Sol: 1 1 0 0 0
% Sol: 0 0 0 0 0
% """
%
% Which is the same solution that fz gives for the minimizing
% objective. The problem has 6 solutions with z = 7.
% 
% z: 7
% 0 1 1 0 1
% 1 0 0 1 0
% 1 1 0 0 0
% 0 0 0 0 0


int: r;  % upper bound for amount of rings
int: n;  % amount of clients


% original comment: 
% we have double entries here because of the symmetric structure!
array[1..n, 1..n] of 0..1: demand;
array[1..r] of 1..n: capacity_nodes;

array[1..r, 1..n] of var 0..1: rings;
var int: z =  sum(ring in 1..r, client in 1..n) (rings[ring, client]);

solve minimize z;
% solve satisfy;


constraint
%   z <= 7 % for solve satisfy
%   /\

   % original comment:
   % if there is a demand between 2 nodes, then there has to exist 
   % a ring, on which they are both installed
   forall(client1,client2 in 1..n where client1 < client2) (
      (demand[client1,client2] = 1) ->
           exists(ring in 1..r) (
                 rings[ring,client1] + rings[ring, client2] >= 2
           )
   )   
   /\
   % original comment:
   % capacity of each ring must not be exceeded     
   forall(ring in 1..r) (
        sum(client in 1..n) (
              rings[ring, client]
              ) <= capacity_nodes[ring] 
   )
;


%
% data
% (sonet_problem1nu.param)
%
r = 4;
n = 5;

demand =
array2d(1..n, 1..n,
        [0,1,0,1,0,
         1,0,1,0,0,
         0,1,0,0,1,
         1,0,0,0,0,
         0,0,1,0,0])
;

capacity_nodes = [3,2,2,1];


output 
[
  "z: ", show(z)
] ++
[
  if client = 1 then "\n" else " " endif ++
    show(rings[ring, client])
  | ring in 1..r, client in 1..n
] ++ ["\n"];
