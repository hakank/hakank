/*

  General planner in SWI Prolog
  
  Inspired by "Thinking as computation", plan.pl and bplan.pl)

  Note that reachable/3 is tabled which makes most plans quite fast
  (or at least probably faster).
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- module(bplan,
          [
           plan/1,
           bplan/1      
           ]).

:- table reachable/3.

%%
%% plan(L):
%% L is a list of moves from the initial state to a goal state.
%%
plan(L) :-
        initial_state(I),
        goal_state(G),
        reachable(I,L,G).

%%%
%%% bplan(L) holds if L is a plan.
%%
%%% This looks for plans, short ones first, using the plan/1 predicate.
%%
bplan(L) :-
        tryplan([],L).

%%
%% tryplan(X,L): L is a plan and has X as its final elements.
%%
tryplan(L,L) :- plan(L).
tryplan(X,L) :- tryplan([_|X],L).

%%
%% reachable(S1,L,S2): S2 is reachable from S1 using moves L.
%%
%% Note: reachable/3 is tabled.
%%
reachable(S,[],S).           
reachable(S1,[M|L],S3) :-
        legal_move(S1,M,S2),
        reachable(S2,L,S3).
