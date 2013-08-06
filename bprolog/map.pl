/*

  Map coloring in B-Prolog.

  Simple map coloring problem.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        connections(Connections), 
        NumColors = 4,
        map(Connections, Countries, NumColors),
        writeln(Countries),

        findall(Countries2, map(Connections, Countries2, NumColors),All),
        length(All, Len),
        format('It was ~d different solutions using ~d colors\n', [Len,NumColors]).

%
% optimize the number of used colors
%
go2 :-
        connections(Connections), 
        NumColors = 5,
        map2(Connections, Countries, NumColors, MinColors),
        writeln(Countries),
        format('We used ~d colors\n', [MinColors]).



map(Connections, Countries, NumColors) :-

        % Note ^length is accessed via @=
        N @= Connections^length, 
        length(Countries, N),
        Countries :: 1..NumColors,
        foreach(C1 in 1..N, C2 in C1+1..N,
                (
                    Connections[C1,C2] =:= 1 -> 
                        Countries[C1] #\= Countries[C2]
                ;
                        true
                )
               ),
        labeling(Countries).


% optimization
map2(Connections, Countries, NumColors, MinColors) :-

        % Note ^length is accessed via @=
        N @= Connections^length, 
        length(Countries, N),
        Countries :: 1..NumColors,
        MinColors :: 1..NumColors, % to optimize
        MinColors #= max(Countries),
        foreach(C1 in 1..N, C2 in C1+1..N,
                (
                    Connections[C1,C2] =:= 1 -> 
                        Countries[C1] #\= Countries[C2]
                ;
                        true
                )
               ),
        minof(labeling(Countries), MinColors).


% Connections between these countries:
% [belgium, denmark, france, germany, netherlands, luxembourg]
connections([]([](0, 0, 1, 1, 1, 1),
               [](0, 0, 0, 1, 0, 0),
               [](1, 0, 0, 1, 1, 0),
               [](1, 1, 1, 0, 1, 1),
               [](1, 0, 1, 1, 0, 0),
               [](1, 0, 0, 1, 0, 0))).
     


