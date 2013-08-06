/*

  K4P2 Graceful Graph in ECLiPSe.

  Problem from Minion summer_school/examples/K4P2GracefulGraph.eprime
  Also see
  http://mathworld.wolfram.com/GracefulGraph.html

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/K4P2GracefulGraph2.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/K4P2GracefulGraph2.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
:-lib(listut).
%:-lib(propia).



go :-
        M = 16,
        N = 8,

        graph(Graph),
        
        % array[1..n] of var 0..m: nodes :: is_output;
        length(Nodes,N),
        Nodes :: 0..M,

        % array[1..m] of var 1..m: edges :: is_output;
        length(Edges,M),
        Edges :: 1..M,

        alldifferent(Edges),
        alldifferent(Nodes),

        ( foreach([From,To],Graph),
          foreach(Edge,Edges),
          param(Nodes) do
              nth1(From,Nodes,FromN),
              nth1(To,Nodes,ToN),
              abs(FromN - ToN) #= Edge
        ),

        term_variables([Nodes,Edges],Vars),
        search(Vars,0, occurrence, indomain_min, complete, [backtrack(Backtracks)]),

        write(nodes:Nodes),nl,
        write(edges:Edges),nl,
        write(backtracks:Backtracks),nl.


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
