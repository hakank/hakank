/*
  1D Rubik's Cube in B-Prolog.
  
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


  Model created by Hakan Kjellerstrand, hakank@gmail.com  
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

go :-       
        time(once(bplan(L))),
        write(L), nl,
        length(L, Len),
        write(len:Len),nl.

go2 :-
        foreach(Len in 1..20,[L,All,AllLen],
                (
                  nl,
                  write(len:Len), nl,
                  length(L, Len),
                  (findall(L, (length(L,Len), plan(L)),All) ; true),
                  write(All),nl,
                  length(All, AllLen),
                  write(all_len:AllLen), nl                  
                )).


go3 :-
        time(findall(L,(length(L,10),plan(L)), All)), length(All,AllLen),
        writeln(All),nl,
        writeln(allLen:AllLen).

% Note: This is a boost!
% The length 8 problems take 1.7s instead of 4s
:-table legal_move/3.


% Length 6 (original problem)

%% All these works in ECLiPSe, SWIPL, SICStus, and B-Prolog
% initial_state([1,3,2,6,5,4]). % Moves: [2,3,2]
% initial_state([5,6,2,1,4,3]). % Moves: [1,3]
% initial_state([6,5,4,1,2,3]). % Moves: [1,2,3]
% initial_state([2,1,5,4,3,6]).  % Moves: 1,2,1

% initial_state([5,1,2,3,4,6]).  % Moves: 1,2,1,2
% initial_state([5,4,3,2,1,6]). % Moves: 1,2,1,2,1

%% These two takes 11 steps (no problem at all).
% initial_state([6,3,5,2,4,1]).   % GAP: x3*x1*x2*x1*x3*x2*x1*x2*x1*x3*x1
% initial_state([6,4,2,5,3,1]).   % GAP: x1*x3*x2*x3*x2*x1*x3*x2*x3*x2*x1

% initial_state([_,_,_,_,_,_]).

% initial_state([6,5,4,3,1,2]). % [1, 3, 2, 1, 3, 1, 2, 1]

% initial_state([6,3,4,5,2,1]). 

% goal_state([1,2,3,4,5,6]).

%% legal_move(From, Move, To).
% legal_move([M4,M3,M2,M1,M5,M6], 1, [M1,M2,M3,M4,M5,M6]). % move 1
% legal_move([M1,M5,M4,M3,M2,M6], 2, [M1,M2,M3,M4,M5,M6]). % move 2
% legal_move([M1,M2,M6,M5,M4,M3], 3, [M1,M2,M3,M4,M5,M6]). % move 3

%
% Length 8
%

%% found in 1.74s
%% find all 27 solutions in 6.4s
%% time(findall(L,(length(L,10),plan(L)), All)), length(All,L).

% initial_state([2,4,1,7,5,3,8,6]). % GAP: x2*x3*x2*x4*x3*x5*x4*x1*x2*x1

% found in 1.72.1s
% finds all 44 solution takes 6.4s
% initial_state([8,7,6,3,2,5,4,1]).  % x3*x1*x2*x3*x1*x4*x5*x1*x3*x1

% legal_move([M4,M3,M2,M1,M5,M6,M7,M8],1,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 1
% legal_move([M1,M5,M4,M3,M2,M6,M7,M8],2,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 2
% legal_move([M1,M2,M6,M5,M4,M3,M7,M8],3,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 3
% legal_move([M1,M2,M3,M7,M6,M5,M4,M8],4,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 4
% legal_move([M1,M2,M3,M4,M8,M7,M6,M5],5,[M1,M2,M3,M4,M5,M6,M7,M8]). % move 5

% goal_state([1,2,3,4,5,6,7,8]).


%
% Length 12
%
% But this takes too long...
%
legal_move([M4,M3,M2,M1,M5,M6,M7,M8,M9,M10,M11,M12],1,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 1
legal_move([M1,M5,M4,M3,M2,M6,M7,M8,M9,M10,M11,M12],2,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 2
legal_move([M1,M2,M6,M5,M4,M3,M7,M8,M9,M10,M11,M12],3,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 3
legal_move([M1,M2,M3,M7,M6,M5,M4,M8,M9,M10,M11,M12],4,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 4
legal_move([M1,M2,M3,M4,M8,M7,M6,M5,M9,M10,M11,M12],5,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 5
legal_move([M1,M2,M3,M4,M5,M9,M8,M7,M6,M10,M11,M12],6,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 6
legal_move([M1,M2,M3,M4,M5,M6,M10,M9,M8,M7,M11,M12],7,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 7
legal_move([M1,M2,M3,M4,M5,M6,M7,M11,M10,M9,M8,M12],8,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 8
legal_move([M1,M2,M3,M4,M5,M6,M7,M8,M12,M11,M10,M9],9,[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12]). % move 9

%% number of moves: 12
initial_state([7,5,11,8,9,1,10,3,4,2,6,12]).
%% number of moves: 12
% initiial_state([12,2,7,3,4,11,1,10,8,9,6,5]).

goal_state([1,2,3,4,5,6,7,8,9,10,11,12]).


%
% General planners (inspired by "Thinking as computation", plan.pl and bplan.pl)
%

% plan(L): L is a list of moves from the initial state to a goal state.
plan(L) :- initial_state(I), goal_state(G), reachable(I,L,G).

% reachable(S1,L,S2): S2 is reachable from S1 using moves L.
reachable(S,[],S).           
reachable(S1,[M|L],S3) :- legal_move(S1,M,S2), reachable(S2,L,S3).

% hakank: This is my version to exclude the same move twice.
% reachable(S1,[M|L],S3) :- legal_move(S1,M,S2), not_same(M,L), reachable(S2,L,S3).

% This looks for plans, short ones first, using the plan predicate.
% bplan(L) holds if L is a plan.
bplan(L) :- tryplan([],L). 

% tryplan(X,L): L is a plan and has X as its final elements.
tryplan(L,L) :- plan(L).
tryplan(X,L) :- tryplan([_|X],L).

not_same(_M,[]).
not_same(_M,[_H]).
not_same(M,[H|_Rest]) :- M \== H.

