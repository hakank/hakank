/*

  Map coloring in SWI Prolog

  Simple map coloring problem of belgium, denmark, france, germany, netherlands, and
  luxembourg.
  go2/0 is an optimization problem.
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Plain coloring
%%
go :-
        connections(Connections), 
        NumColors = 4,
        map_color(Connections, Countries, NumColors),
        writeln(coloring=Countries),

        findall(Countries2, map_color(Connections, Countries2, NumColors),All),
        length(All,Len),
        format("It was ~d different solutions using ~d colors:\n", [Len,NumColors]),
        writeln(All).


%%
%% optimize the number of used colors
%%
go2 :-
        connections(Connections), 
        NumColors = 15,
        map_color2(Connections, Countries, NumColors, MinColors),
        writeln(Countries),
        format("We used ~d colors\n", [MinColors]).

%%
%% Simple coloring.
%%
map_color(Connections, Countries, NumColors) :-

        length(Connections,N),
        length(Countries,N),
        Countries ins 1..NumColors,
        
        findall([A,B], (between(1,N,A),
                        between(1,A,B),
                        nth1(A,Connections,Conn),
                        nth1(B,Conn,1)
                       ),
                ABs),
        maplist(not_same_color(Countries),ABs),
        
        %% symmetry breaking
        element(1,Countries,1),
        element(2,Countries,C2),
        C2 #=< 2,

        label(Countries).

not_same_color(Countries,[A,B]) :-
        element(A,Countries,CA),
        element(B,Countries,CB),
        CA #\= CB.

%%
%% Optimization: minimize the number of colors needed.
%%
map_color2(Connections, Countries, NumColors, MinColors) :-

        length(Connections,N),
        
        length(Countries,N),
        Countries ins 1..NumColors,
        MinColors in 1..NumColors, %% to optimize

        %% minimize the max number of color
        max_list_clp(Countries,MinColors),
        
        findall([A,B], (between(1,N,A),
                        between(1,A,B),
                        nth1(A,Connections,Conn),
                        nth1(B,Conn,1)
                       ),
                ABs),
        maplist(not_same_color(Countries),ABs),
        
        %% symmetry breaking
        element(1,Countries,1),
        element(2,Countries,C2),
        C2 #=< 2,

        labeling([min(MinColors)], Countries).


%%
%% Connections between these countries:
%% [belgium, denmark, france, germany, netherlands, luxembourg]
connections(A) :- 
        A = [[0, 0, 1, 1, 1, 1],
             [0, 0, 0, 1, 0, 0],
             [1, 0, 0, 1, 1, 0],
             [1, 1, 1, 0, 1, 1],
             [1, 0, 1, 1, 0, 0],
             [1, 0, 0, 1, 0, 0]].
     


