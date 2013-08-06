/*

  Max flow problem in B-Prolog.

  From Winston "Operations Research", page 420f, 423f
  Sunco Oil example.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-

        Cap =  [2,3,3,4,2,1,100],
        Arcs = [[1, 2],
                [1, 3],
                [2, 3],
                [2, 4],
                [3, 5],
                [4, 5],
                [5, 1]],
        length(Cap,NumArcs),
        N = 5,
        Nodes = 1..N,

        new_array(Flow,[N,N]),
        array_to_list(Flow, FlowVars),
        Z #= Flow[N,1],

        foreach(I in Nodes, J in Nodes, Flow[I,J] #>= 0),
        foreach(I in 1..NumArcs,Flow[Arcs[I,1], Arcs[I,2]] #=< Cap[I]),
        foreach(I in Nodes,
                (sum([Flow[Arcs[K,1], Arcs[K,2]] : K in 1..NumArcs,Arcs[K,1] =:= I])
                 #=
                 sum([Flow[Arcs[K,1], Arcs[K,2]] : K in 1..NumArcs,Arcs[K,2] =:= I])
                )),

        maxof(labeling(FlowVars),Z),

        writeln(z:Z),
        Rows @= Flow^rows,
        foreach(Row in Rows, writeln(Row)),
        nl.