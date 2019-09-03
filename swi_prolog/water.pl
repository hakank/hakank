/*

  Water jugs problem in SWI Prolog

  See https://en.wikipedia.org/wiki/Water_pouring_puzzle
  """
  Water pouring puzzles (also called water jug problems or measuring puzzles) are a class of puzzle
  involving a finite collection of water jugs of known integer capacities (in terms of a liquid
  measure such as liters or gallons). Initially each jug contains a known integer volume of liquid,
  not necessarily equal to its capacity. Puzzles of this type ask how many steps of pouring water
  from one jug to another (until either one jug becomes empty or the other becomes full) are
  needed to reach a goal state, specified in terms of the volume of liquid that must be present in
  some jug or jugs.
  """
  
  The solution:
    fill1=(8,0)
    transfer_1_to_2=(3,5)
    empty2=(3,0)
    transfer_1_to_2=(0,3)
    fill1=(8,3)
    transfer_1_to_2=(6,5)
    empty2=(6,0)
    transfer_1_to_2=(1,5)
    empty2=(1,0)
    transfer_1_to_2=(0,1)
    fill1=(8,1)
    transfer_1_to_2=(4,5)
    len=12


  The bplan module is here: http.//hakank.org/swi_prolog/bplan.pl

  Compare with the following which use a different approach:
  - http.//hakank.org/swi_prolog/3_jugs.pl
  - http.//hakank.org/swi_prolog/3_jugs_regular.pl  

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(bplan).
:- use_module(library(clpfd)).

%%
%% Solve the water jugs problem
%%
go :-
        abolish_all_tables, % bplan tables reachable/3 (legal_moves/3)
        bplan(Plan),
        maplist(writeln,Plan),
        length(Plan,Len),
        writeln(len=Len),
        nl.

%%
%% Initial state: both water jugs are empty
%%
initial_state([0,0]).


%%
%% Goal states: either of the jugs should contain 4 water units
%%
goal_state([4,_]).
goal_state([_,4]).

%%
%% (Max) Capacities of the water jugs.
%%
capacity(1,8).
capcity(2,5).


% goal_state([0,4]). %% a more exact goal

legal_move([_V1,V2],Action,S1) :-
        capacity(1,C),
        Action = (fill1=(C,V2)),
        S1 = [C,V2].

legal_move([V1,_V2],Action,S1) :-
        capacity(2,C),
        Action = (fill2=(V1,C)),        
        S1 = [V1,C].

legal_move([V1,V2],Action,S1) :-
        V1 #> 0,
        Action = (empty1=(0,V2)),
        S1 = [0,V2].

legal_move([V1,V2],Action,S1) :-
        V2 #> 0,
        Action = (empty2=(V1,0)),
        S1 = [V1,0].

legal_move([V1,V2],Action,S1) :-
        V2 #> 0,
        capcity(1,C1),
        Liquid #= V1+V2,
        Excess #= Liquid-C1,
        (Excess #=< 0
        ->
         W1 #= Liquid, W2 #= 0
        ;
         W1 #= C1, W2 #= Excess
        ),
        Action = (transfer_2_to_1=(W1,W2)),
        S1=[W1,W2].

legal_move([V1,V2],Action,S1) :-
        V1 #> 0,
        capcity(2,C2),
        Liquid #= V1+V2,
        Excess #= Liquid-C2,
        (Excess #=< 0
        ->
         W2 #= Liquid, W1 #= 0
        ;
         W2 #= C2, W1 #= Excess
        ),
        Action = (transfer_1_to_2=(W1,W2)),
        S1=[W1,W2].    
