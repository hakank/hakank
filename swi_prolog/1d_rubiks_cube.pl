/*

  1D Rubik's Cube in SWI Prolog

  From
  http://www.mail-archive.com/programming@jsoftware.com/msg05817.html
  """
  1D Rubik's Cube

  Oleg Kobchenko
  Mon, 11 Jun 2007 19:09:55 -0700

  I found an interesting game, as found on Andrew Nikitin's 
  MSX-BASIC page http://nsg.upor.net/msx/basic/basic.htm ,
  and I am not sure if its solver has been given as a puzzle.
  Here it goes.

  1D Rubik's Cube is a line of 6 numbers with
  original position:

    1 2 3 4 5 6

  which can be rotated in 3 different ways
  in groups of four:
      _______                _______
     (1 2 3 4)5 6  --(0)->  (4 3 2 1)5 6
        _______                _______
      1(2 3 4 5)6  --(1)->   1(5 4 3 2)6
          _______                _______
      1 2(3 4 5 6) --(2)->   1 2(6 5 4 3)

  Given a scrambled line, return the shortest sequence of 
  rotations to restore the original position.

  Examples:

     solve 1 3 2 6 5 4
  1 2 1
     solve 5 6 2 1 4 3
  0 2
     solve 6 5 4 1 2 3
  0 1 2

  """


  Here is a GAP program for this problem.
  Note: It actually solves the opposite problem:
  Given a sequence, how to construct it, i.e. the 
  order of operations are reversed:

  Coding: the three operations (reverse) as cycle notation:

    1 2 3 4 5 6      (1,4)(2,3)
    4 3 2 1 5 6     

    1 2 3 4 5 6      (2,5)(3,4)
    1 5 4 3 2 6     

    1 2 3 4 5 6      (3,6)(4,6)
    1 2 6 5 4 3

    Now the GAP code:
  
    gap> g:=Group([(1,4)(2,3), (2,5)(3,4), (3,6)(4,5)]);
    Group([ (1,4)(2,3), (2,5)(3,4), (3,6)(4,5) ])
    gap> Order(g);                                      
    360
    gap> a:=g.1; b:=g.2; c:=g.3;                        
    (1,4)(2,3)
    (2,5)(3,4)
    (3,6)(4,5)
    gap> StructureDescription(g);
    "A6"
    gap> ListPerm(a);
    [ 4, 3, 2, 1 ]
    gap> ListPerm(b);
    [ 1, 5, 4, 3, 2 ]
    gap> ListPerm(c);
    [ 1, 2, 6, 5, 4, 3 ]

    And the three problems:
    gap> Factorization(g,PermList([1,3,2,6,5,4]));
    x2*x3*x2

    gap> Factorization(g,PermList([5,6,2,1,4,3]));
    x1*x3

    gap> Factorization(g,PermList([6,5,4,1,2,3]));
    x1*x2*x3

  
  Here we test both length 6 problems as well as length 8 problems.
  This means that we have to retract/assert both initial states
  and goal_states (perhaps not as pure as it could be).

  This program use the module bplan which must be downloaded:
  http://hakank.org/swi_prolog/bplan.pl (or via http://hakank.org/swi_prolog/ ).


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(bplan).

:- dynamic initial_state/1.
:- dynamic goal_state/1.
:- dynamic legal_move/3.

%%
%% Shortest plan for a length 6 and length 8 problem instance.
%%
go :-
        %% Length 6 problem
        Initial1 = [1,3,2,6,5,4],
        run_problem_once(Initial1),
        nl,
        
        %% Length 8 problem
        Initial = [2,4,1,7,5,3,8,6],
        run_problem_once(Initial),
        nl.

%%
%% Test a couple of length 6 problems
%%
go2 :-
        InitialStates =
        [
         [1,3,2,6,5,4],         % Moves: [2,3,2]
         [5,6,2,1,4,3],         % Moves: [1,3]
         [6,5,4,1,2,3],         % Moves: [1,2,3]
         [2,1,5,4,3,6],         % Moves: 1,2,1
         
         [5,1,2,3,4,6],         % Moves: 1,2,1,2
         [5,4,3,2,1,6],         % Moves: 1,2,1,2,1
         
         %% These two takes 11 steps (no problem at all).
         [6,3,5,2,4,1],        % GAP: x3*x1*x2*x1*x3*x2*x1*x2*x1*x3*x1
         [6,4,2,5,3,1],        % GAP: x1*x3*x2*x3*x2*x1*x3*x2*x3*x2*x1
         
         [6,5,4,3,1,2],        % [1, 3, 2, 1, 3, 1, 2, 1]
         [6,3,4,5,2,1],
         [_,_,_,_,_,_]         % 0 moves
        ],
        
        member(Initial,InitialStates),
        run_problem_once(Initial),
        nl,
        fail,
        nl.

go2.


%%
%% Test some 8 length problems.
%%
go3 :-
        InitialStates = [
                         [2,4,1,7,5,3,8,6],
                         [8,7,6,3,2,5,4,1]
                        ],
        member(Initial,InitialStates),
        run_problem_once(Initial),
        nl,
        fail,
        nl.

go3.

%%
%% Check all plans of a certain length (1..15).
%% N.B. The shortest is (still) of length 10.
%%
%% Note: It just shows the number of solutions.
%%
%% Result (edited):
%%
%% len=1..9: no solutions
%%
%% len=10
%% % 69 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 843696 Lips)
%% number_of_solutions=27
%%
%% len=11
%% % 605,713 inferences, 0.307 CPU in 0.307 seconds (100% CPU, 1972598 Lips)
%% number_of_solutions=131
%%
%% len=12
%% % 823,096 inferences, 0.436 CPU in 0.436 seconds (100% CPU, 1889367 Lips)
%% number_of_solutions=1976
%%
%% len=13
%% % 1,028,973 inferences, 0.599 CPU in 0.599 seconds (100% CPU, 1716804 Lips)
%% number_of_solutions=9469
%%
%% len=14
%% % 2,040,996 inferences, 1.189 CPU in 1.189 seconds (100% CPU, 1716202 Lips)
%% number_of_solutions=89196
%%
%% len=15
%% % 6,343,194 inferences, 3.474 CPU in 3.474 seconds (100% CPU, 1826040 Lips)
%% number_of_solutions=427105
%%
go4 :-
        Initial = [2,4,1,7,5,3,8,6],
        between(1,15,Len),
        writeln(len=Len),
        nl,
        run_problem_findall(Initial,Len,false),
        nl,
        fail,
        nl.

go4.

%%
%% Length 6 problems.
%%

%%
%% legal_move(From, Move, To).
%%
%% Legal moves for length 6 problems.
%%
legal_move6([M4,M3,M2,M1,M5,M6], 1, [M1,M2,M3,M4,M5,M6]). % move 1
legal_move6([M1,M5,M4,M3,M2,M6], 2, [M1,M2,M3,M4,M5,M6]). % move 2
legal_move6([M1,M2,M6,M5,M4,M3], 3, [M1,M2,M3,M4,M5,M6]). % move 3

%% goal_state([1,2,3,4,5,6]). %% dynamically asserted

%%
%% Length 8 problems
%%

%%
%% Legal moves for length 8 problems.
%%
legal_move8([M4,M3,M2,M1,M5,M6,M7,M8],1,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 1
legal_move8([M1,M5,M4,M3,M2,M6,M7,M8],2,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 2
legal_move8([M1,M2,M6,M5,M4,M3,M7,M8],3,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 3
legal_move8([M1,M2,M3,M7,M6,M5,M4,M8],4,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 4
legal_move8([M1,M2,M3,M4,M8,M7,M6,M5],5,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 5

%% goal_state([1,2,3,4,5,6,7,8]). %% dynamically asserted


%%
%% run_problem_once(Initial)
%%
%% Wrapper for running a problem once, finding an/the optimal/shortest plan.
%%
run_problem_once(Initial) :-
        writeln(initial_state=Initial),
        retractall(initial_state(_)),
        assertz(initial_state(Initial)),
        length(Initial, InitialLen),
        writeln(initialLen=InitialLen),
        assert_legal_moves(InitialLen),
        
        time(once(bplan(L))),
        writeln(moves=L), 
        length(L, Len),
        writeln(len=Len),
        retractall(initial_state(_)),
        nl.

%%
%% run_problem_findall(Initial,Len)
%% run_problem_findall(Initial,Len,ShowSolutions)
%%
%% Wrapper for finding all plans of length Len.
%%
%%
run_problem_findall(Initial,Len) :-
        run_problem_findall(Initial,Len,true).

run_problem_findall(Initial,Len,ShowSolutions) :-
        writeln(initial_state=Initial),
        retractall(initial_state(_)),        
        assertz(initial_state(Initial)),
        length(Initial,InitialLen),
        writeln(initialLen=InitialLen),
        assert_legal_moves(InitialLen),

        %% create a list of the appropriate plan length
        length(L,Len),
        time(findall(L, plan(L),All)),
        (
         ShowSolutions == true
        ->
         writeln(All)
        ;
         true
        ),
        length(All, AllLen),
        writeln(number_of_solutions=AllLen),
        retractall(initial_state(_)),        
        nl.

%%
%% Assert goal_state/1 and legal_move/3 for the different lengths.
%%

%% Length 6
assert_legal_moves(6) :-
        abolish_all_tables,
        retractall(goal_state(_)),
        numlist(1,6,Goal),
        writeln(goal=Goal),
        assertz(goal_state(Goal)),
        retractall(legal_move(_,_,_)),        
        legal_move6(From,Move,To),
        assertz(legal_move(From,Move,To)),
        fail.
assert_legal_moves(6).

%% Length 8
assert_legal_moves(8) :-
        abolish_all_tables,
        retractall(goal_state(_)),
        numlist(1,8,Goal),
        assertz(goal_state(Goal)),
        retractall(legal_move(_,_,_)),
        legal_move8(From,Move,To),
        assertz(legal_move(From,Move,To)),
        fail.
assert_legal_moves(8).

