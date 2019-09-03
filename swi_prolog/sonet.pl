/*

  SONET problem in SWI Prolog

  From the ESSENCE' model in the Minion Translator examples:
  http://www.cs.st-andrews.ac.uk/~andrea/examples/sonet/sonet_problem.eprime
  """
  The SONET problem is a network design problem: set up a network between
  n nodes, where only certain nodes require a connection.
  Nodes are connected by putting them on a ring, where all nodes
  on a ring can communicate. Putting a node on a ring requires a so-called
  ADM, and each ring has a capacity of nodes, i.e. ADMs. There is a certain 
  amount of rings, r, that is available. The objective is to set up a network
  by using a minimal amount of ADMs.

  About the problem model

  The problem model has the amount of rings ('r'), amount of nodes('n'),
  the 'demand' (which nodes require communication) and node-capacity of each 
  ring ('capacity_nodes') as parameters.
  The assignement of nodes to rings is modelled by a 2-dimensional matrix 'rings',
  indexed by the amnount of rings and nodes. The matrix-domain is boolean:
  If the node in column j is assigned to the ring in row i, then rings[i,j] = 1 
  and 0 otherwise. So all the '1's in the matrix 'rings' stand for an ADM.
  Hence the objective is to minimise the sum over all columns and rows of matrix
  'rings'.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        R = 4,
        N = 5,
        Demand = [[0,1,0,1,0],
                  [1,0,1,0,0],
                  [0,1,0,0,1],
                  [1,0,0,0,0],
                  [0,0,1,0,0]],

        CapacityNodes = [3,2,2,1],

        %% decision variables
        new_matrix(R,N, 0..1, Rings),
        flatten(Rings,Vars),

        %% to optimize
        sum(Vars,#=,Z),
        
        %% if there is a demand between 2 nodes, then there has to exist 
        %% a ring, on which they are both installed
        common_rings(N,Demand,Rings),
        
        %% capacity of each ring must not be exceeded     
        maplist(capacity_ring,Rings,CapacityNodes),
        
        labeling([min(Z)],Vars),

        writeln(z=Z),
        maplist(writeln,Rings),
        nl.

%% if there is a demand between 2 nodes, then there has to exist 
%% a ring, on which they are both installed
common_rings(N,Demand,Rings) :-
        findall([Client1,Client2],
                (between(1,N,Client1),
                 Client1_1 #= Client1+1,
                 between(Client1_1, N,Client2)
                ),
                Cs),
        maplist(common_rings_(Rings,Demand),Cs).
common_rings_(Rings,Demand,[Client1,Client2]) :-
        matrix_element5(Demand,Client1,Client2,D),
        (
         D #= 1
        ->
         matrix_element5(Rings,Ring,Client1,R1),
         matrix_element5(Rings,Ring,Client2,R2),
         R1 + R2 #>= 2
        ;
         true
        ).

%%
%% capacity of each ring must not be exceeded
%%
capacity_ring(RingsRow,CapacityNode) :-
        sum(RingsRow,#=,S),
        S #=< CapacityNode.
        