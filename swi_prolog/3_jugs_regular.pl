/*

  3 jugs problem using regular constraint in SWI Prolog

  Using the regular constraint and DFS to solve the 3 jugs problem.

  Also see http://mathworld.wolfram.com/ThreeJugProblem.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Using indomain to get the shortest path
%%
%% ...
%% len=7
%% x=[9,10,11,12,13,14,15]
%% path=[1,9,10,11,12,13,14,15]
%%
%% The path:
%% state: 1  move: [8,0,0]
%% state: 9  move: [3,5,0]
%% state: 10  move: [3,2,3]
%% state: 11  move: [6,2,0]
%% state: 12  move: [6,0,2]
%% state: 13  move: [1,5,2]
%% state: 14  move: [1,4,3]
%% state: 15  move: [4,4,0]
%%
go :-

        dfa(1,Transition,NStates,InputMax,InitialState,AcceptingStates,Nodes),
        
        %% get the minimum length by checking increasing length of X
        Len in 1..InputMax,
        indomain(Len),
        writeln(len=Len),

        length(X,Len),
        X ins 0..InputMax,

        regular2(X, NStates, InputMax, Transition, InitialState, AcceptingStates,Path),

        labeling([],X),

        writeln(x=X),
        writeln(path=Path),
        nl,
        writeln("The path:"),
        findall([S,Node],
                ( member(S,Path),
                  nth1(S,Nodes,Node)
                ),
                Paths),
        maplist(format("state: ~w  move: ~w~n"),Paths),
        nl.


%%
%% Note: The solution for this model is
%% 8 steps where X =  [2,3,4,5,6,7,8,15,15,15,15,15,15,15,15]
%% which is clealy wrong. The optimal result is 7 steps, see above.
%%
%% One problem seems to be that X is already assigned to [2,3,4,5,6,7,8,15,15,15,15,15,15,15,15]
%% before the labeling, so min(Cost) cannot backtrack to the correct result.
%%
go2 :-

        dfa(1,Transition,NStates,InputMax,InitialState,AcceptingStates,Nodes),       
        length(X,InputMax),
        X ins 0..InputMax,

        %% Find the shortest path
        %% Cost #= 1+sum([ X[I-1] #!= X[I] : I in 2..InputMax]),
        Cost in 0..NStates,
        jugs_cost(X,NStates,Cost),

        % Cost #= 7,

        %% Using regular2/7 to get the Path which simplifies the printing of the solution.
        regular2(X, NStates, InputMax, Transition, InitialState, AcceptingStates,Path),

        writeln(xbefore_labeling=X), 
        writeln(cost_before_labeling=Cost),       
        labeling([ff,min(Cost)],X),

        writeln(cost=Cost),
        writeln(x=X),
        writeln(path=Path),
        findall([S,Node],
                ( member(S,Path),
                  nth1(S,Nodes,Node)
                ),
                Paths),
        maplist(format("state: ~w  move: ~w~n"),Paths),
        
        nl.


%% Cost #= 1+sum([ X[I-1] #!= X[I] : I in 2..InputMax]),
jugs_cost(X,N,Cost) :-
        numlist(2,N,Is),
        sumx(Is,X, 0,Cost1),
        Cost #= Cost1 + 1.

sumx([],_X,Sum,Sum).
sumx([I|Is],X,Sum0,Sum) :-
        element(I,X,XI),
        I1 #= I-1,
        element(I1,X,XI1),
        B in 0..1, %% don't forget the domain of B!
        (B #= 1) #<==> (XI #\= XI1),
        Sum1 #= Sum0 + B,
        sumx(Is,X,Sum1,Sum).
        

dfa(1,Transition,NStates,InputMax,InitialState,AcceptingStates,Nodes) :-
        Transition = 
        [ %%1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
          [ 0, 2, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0], % 1 Initial state
          [ 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], % 2 
          [ 0, 0, 0, 4, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0], % 3
          [ 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], % 4
          [ 0, 0, 0, 0, 0, 6, 0, 0, 9, 0, 0, 0, 0, 0, 0], % 5
          [ 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0], % 6
          [ 0, 0, 0, 0, 0, 0, 0, 8, 9, 0, 0, 0, 0, 0, 0], % 7
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15], % 8 
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0,10, 0, 0, 0, 0, 0], % 9
          [ 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,11, 0, 0, 0, 0], %10
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12, 0, 0, 0], %11 
          [ 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,13, 0, 0], %12
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,14, 0], %13 
          [ 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15], %14
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15] %15 Accepting state
        ],

        NStates = 15,
        InputMax = 15,
        InitialState = 1,
        AcceptingStates = [15],

        Nodes = [
                 ["8,0,0"],     % 1 start
                 ["5,0,3"],     % 2
                 ["5,3,0"],     % 3 
                 ["2,3,3"],     % 4 
                 ["2,5,1"],     % 5
                 ["7,0,1"],     % 6
                 ["7,1,0"],     % 7
                 ["4,1,3"],     % 8
                 ["3,5,0"],     % 9
                 ["3,2,3"],     % 10
                 ["6,2,0"],     % 11
                 ["6,0,2"],     % 12
                 ["1,5,2"],     % 13
                 ["1,4,3"],     % 14
                 ["4,4,0"]      % 15 goal
                ].

