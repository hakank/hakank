/*

  Ormat game in B-Prolog.

  From bit-player "The Ormat Game"
  http://bit-player.org/2010/the-ormat-game
  """
  I'm going to give you a square grid, with some of the cells colored 
  and others possibly left blank. We'll call this a template. Perhaps 
  the grid will be one of these 3x3 templates:
  
  [see pictures at the web page]
  
  You have a supply of transparent plastic overlays that match the 
  grid in size and shape and that also bear patterns of black dots:
  
  [ibid.]

  Your task is to assemble a subset of the overlays and lay them on 
  the template in such a way that dots cover all the colored squares 
  but none of the blank squares. You are welcome to superimpose multiple 
  dots on any colored square, but overall you want to use as few overlays 
  as possible. To make things interesting, I'll suggest a wager. I'll pay 
  you $3 for a correct covering of a 3x3 template, but you have to pay me 
  $1 for each overlay you use. Is this a good bet?
  """
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).


go :-
        % Problem 6 is the hardest and takes about 7s
        % using ip_solve. The other problem are solved in <1s.
        foreach(P in 1..7, time2(ormat_game(P))).


% Problem 8 is the hardest
go2 :-
        time2(ormat_game(8)).


% generate the 4! overlays
go3 :-
        generate(4, Overlays),
        foreach(O in Overlays, [I], (print_grid(O,I),nl)),
        length(Overlays,Len),
        writeln(len:Len),
        nl.

% generate the (adjusted) overlays for Problem 4
go4 :-
        problem(6,N, Problem),
        generate(N, Problem, Overlays),
        foreach(O in Overlays, [I], (print_grid(O,I),nl)),
        length(Overlays,Len),
        writeln(len:Len),
        nl.

% How much is reduced for the problems using generate/3?
go5 :-
        foreach(P in 1..8,
                [Problem, N,Len,Overlays,Fact,Reduction],
                (problem(P,N,Problem),
                 factorial(N,Fact),
                 writeln([problem(P),n:N,fact:Fact]),
                 generate(N,Problem,Overlays),
                 length(Overlays,Len),
                 writeln(num_overlays:Len),
                 Reduction is 1-(Len / Fact),
                 writeln(reduction_factor:Reduction),
                 nl
                )
               ).

factorial(N,F) :-
        foreach(I in 1..N, ac(F,1), F^1 #= I*F^0).

ormat_game(P) :-

        problem(P, N, Problem),

        format('Problem: ~d\n', [P]),
        print_grid(Problem,_),
        nl,

        % There are N! possible overlays
        % generate(N,Overlays),

        % This version of generate reduces the tentative overlays.
        generate(N,Problem,Overlays),
        OSize @= Overlays^length,

        % decision variables
        length(X,OSize),
        X :: 0..1,
        NumOverlays $= sum(X),

        % constraints

        % The MIP solver likes this...
        foreach(I in 1..N, J in 1..N,
                (
                    Problem[I,J] =:= 1 ->
                     sum([X[O]*Overlays[O,I,J] : O in 1..OSize]) $>=  1
                ;
                     sum([X[O]*Overlays[O,I,J] : O in 1..OSize]) $= 0
                )),

        % minimize the number of overlays needed

        % minof(labeling([split], Vars), NumOverlays),
        % cp_solve([ff,min(NumOverlays)], X),
        ip_solve([min(NumOverlays)], X), % this is the fastest
        % sat_solve([min(NumOverlays)], X),

        % output
        writeln(x:X),
        writeln(numOverlays:NumOverlays),
        writeln('\nThe overlays:'),
        foreach(I in 1..OSize,
                [Grid],
                (X[I] =:= 1 ->
                     Grid @= Overlays[I],
                     print_grid(Grid,I),
                     nl
                ;
                     true
                )),
        writeln(numOverlays:NumOverlays),
        nl.


print_grid(Grid,I) :-
        (nonvar(I) -> 
             format("#~d\n",[I])
        ;
             true
        ),
        foreach(G in Grid, writeln(G)).



%
% generate the N! overlays
%
generate(N,Overlays) :-
        findall(Overlay,generate1(N,Overlay), Overlays).

generate1(N,Overlay) :-
        new_array(OverlayA, [N,N]),
        array_to_list(OverlayA,Vars),
        Vars :: 0..1,
        foreach(I in 1..N,
                (sum([OverlayA[I,J] : J in 1..N]) #= 1,
                 sum([OverlayA[J,I] : J in 1..N]) #= 1 
                )),
        labeling([ff,split],Vars),
        % convert to list
        Overlay @= OverlayA^rows.


% This version checks that it is a possible overlay,
% i.e. that is: when then problem has and 0 then the
% overlay also must have an 0 in the same position.
% This reduces the number of overlays and the complexity 
% of the problem
generate(N,Problem,Overlays) :-
        findall(Overlay,generate1(N,Problem,Overlay), Overlays).

generate1(N,Problem,Overlay) :-
        new_array(OverlayA, [N,N]),
        array_to_list(OverlayA,Vars),
        Vars :: 0..1,
        foreach(I in 1..N,
                (foreach(J in 1..N, 
                         Problem[I,J] #= 0 #=> OverlayA[I,J] #= 0),
                    sum([OverlayA[I,J] : J in 1..N]) #= 1,
                    sum([OverlayA[J,I] : J in 1..N]) #= 1
                )),
        labeling([ff,split],Vars),
        % convert to list
        Overlay @= OverlayA^rows.



% The following problems are from 
% http://bit-player.org/2010/the-ormat-game
%
% Problem grid 1 (n=3)
problem(1,3,
        [[1,0,0],
         [0,1,1],
         [0,1,1]]).

% Problem grid 2 (n=3)
problem(2,3,
        [[1,1,1],
         [1,1,1],
         [1,1,1]
        ]).

% Problem grid 3 (n=3)
problem(3,3,
        [[1,1,1],
         [1,1,1],
         [1,1,0]]).


%
% Problem 4 (n=4)
%
problem(4,4,
        [[1,1,1,1],
         [1,1,1,1],
         [1,1,1,1],
         [1,1,0,0]]).


%
% Problem 5 (n=5)
%
problem(5,5,
        [[1,1,1,1,1],
         [1,1,1,1,1],
         [1,1,1,1,1],
         [1,1,1,1,1],
         [1,1,0,0,0]]).
        

%
% Problem 6 (n=6)
%
% hard (~7s with ip_solve)
%
problem(6,6,
        [[1,1,1,1,1,1],
         [1,1,1,1,1,1],
         [1,1,1,1,1,1],
         [1,1,1,1,1,1],
         [1,1,1,1,1,1],
         [1,1,0,0,0,0]]).


%
% Problem 7 (n=7)
%
problem(7,7,
        [[1,1,1,1,1,1,1],
         [1,1,1,1,1,1,1],
         [0,1,1,1,1,1,1],
         [0,0,1,1,1,1,1],
         [0,0,0,1,1,1,1],
         [0,0,0,0,1,1,1],
         [0,0,0,0,0,1,1]]).

%
% Problem 8 (n=7), "flags"
%
% quite hard: ~1:50 minutes with ip_solve
%
problem(8,7,
        [[0,0,0,1,1,1,1],
         [0,0,0,1,1,1,1],
         [0,0,0,1,1,1,1],
         [1,1,1,1,1,1,1],
         [1,1,1,1,1,1,1],
         [1,1,1,1,1,1,1],
         [1,1,1,1,1,1,1]]).
