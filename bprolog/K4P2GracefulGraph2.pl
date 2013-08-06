/*

  K4P2 Graceful Graph in B-Prolog.

  Problem from Tailor/Minion summer_school/examples/K4P2GracefulGraph.eprime
  Also see
  http://mathworld.wolfram.com/GracefulGraph.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        M = 16,
        N = 8,
        graph(Graph),
        graceful_graph(Graph, M,N,Nodes,Edges),
        write(nodes:Nodes),nl,
        write(edges:Edges),nl.

% Just count the number of solutions
go2 :-
        M = 16,
        N = 8,
        graph(Graph),
        findall(_, graceful_graph(Graph, M,N,_Nodes,_Edges),L),
        length(L, Len),
        format("Number of solutions: ~d\n",[Len]).
        

graceful_graph(Graph, M,N,Nodes,Edges) :-

        length(Nodes,N),
        Nodes :: 0..M,

        length(Edges,M),
        Edges :: 1..M,

        alldifferent(Edges),
        alldifferent(Nodes),

        foreach(([From,To],Edge) in (Graph,Edges),
                [FromN,ToN],
                (
                    element(From,Nodes,FromN),
                    element(To,Nodes,ToN),
                    abs(FromN - ToN) #= Edge
                )
        ),

        term_variables([Nodes,Edges],Vars),
        labeling(Vars).



graph([[1, 2],
       [1, 3],
       [1, 4],
       [2, 3],
       [2, 4],
       [3, 4],
       
       [5, 6],
       [5, 7],
       [5, 8],
       [6, 7],
       [6, 8],
       [7, 8],
       
       [1, 5],
       [2, 6],
       [3, 7],
       [4, 8]]).
