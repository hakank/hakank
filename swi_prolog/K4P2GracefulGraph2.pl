/*

  K4P2 Graceful Graph in SWI Prolog

  Problem from Tailor/Minion summer_school/examples/K4P2GracefulGraph.eprime
  Also see
  http://mathworld.wolfram.com/GracefulGraph.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        graph(Graph),        
        length(Graph,M),
        flatten(Graph,GraphF),
        max_list(GraphF,N),
        writeln([n=N,m=M]),

        graceful_graph(Graph, M,N,Nodes,Edges),
        writeln(nodes=Nodes),
        writeln(edges=Edges).


%% Count the number of solutions
%%
%% Number of solutions: 1440
%% % 6,083,184,095 inferences, 428.301 CPU in 428.299 seconds (100% CPU, 14203065 Lips)
%%
go2 :-
        graph(Graph),        
        length(Graph,M),
        flatten(Graph,GraphF),
        max_list(GraphF,N),
        writeln([n=N,m=M]),       
        findall(_, graceful_graph(Graph, M,N,_Nodes,_Edges),L),
        length(L,Len),
        format("Number of solutions: ~d~n", [Len]).


graceful_graph(Graph, M,N,Nodes,Edges) :-

        length(Nodes,N),
        Nodes ins 0..M,

        length(Edges,M),
        Edges ins 1..M,

        all_distinct(Edges),
        all_distinct(Nodes),

        maplist(graph_edges(Nodes),Graph,Edges),
        
        flatten([Nodes,Edges],Vars),
        labeling([bisect],Vars).

graph_edges(Nodes,[From,To],Edge) :-
        element(From,Nodes,NodesFrom),
        element(To,Nodes,NodesTo),        
        abs(NodesFrom - NodesTo) #= Edge.


graph(Graph) :- 
        Graph = 
        [[1, 2],
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
         [4, 8]].
