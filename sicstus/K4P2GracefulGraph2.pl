/*

  K4P2 Graceful Graph in SICStus Prolog.
 
  Problem from Minion summer_school/examples/K4P2GracefulGraph.eprime
  Also see
  http://mathworld.wolfram.com/GracefulGraph.html

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/K4P2GracefulGraph2.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        M = 16,
        N = 8,

        graph(Graph),
        
        % array[1..n] of var 0..m: nodes :: is_output;
        length(Nodes,N),
        domain(Nodes,0,M),

        % array[1..m] of var 1..m: edges :: is_output;
        length(Edges,M),
        domain(Edges,1,M),

        all_different(Edges),
        all_different(Nodes),

        ( foreach([From,To],Graph),
          foreach(Edge,Edges),
          param(Nodes) do
              element(From,Nodes,FromN),
              element(To,Nodes,ToN),
              abs(FromN - ToN) #= Edge
        ),

        append(Nodes,Edges,Vars),
        labeling([leftmost,step,up],Vars),

        write(nodes:Nodes),nl,
        write(edges:Edges),nl,nl,
        fd_statistics.


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
