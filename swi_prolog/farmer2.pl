/*

  Farmer problem in SWI Prolog

  This is a port of the B-Prolog program
  http://www.picat-lang.org/bprolog/examples/tabling/farmer.pl

  Solution:
  [farmer_goat,farmer_alone,farmer_cabbage,farmer_goat,farmer_wolf,farmer_alone,farmer_goat]
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
    S0=[s,s,s,s],
    plan(S0,Plan,_),
    writeln(Plan).

final([n,n,n,n]).

:-table plan(+,-,min).
plan([n,n,n,n],Plan,Len) :-
        Plan=[], Len=0.
plan(S,Plan,Len) :-
        Plan=[Action|Plan1],
        action(S,S1,Action),
        plan(S1,Plan1,Len1),
        Len is Len1+1.

action([F,F,G,C],S1,Action) :-
        Action=farmer_wolf,
        opposite(F,F1),
        S1=[F1,F1,G,C],
        \+ unsafe(S1).
action([F,W,F,C],S1,Action) :-
        Action=farmer_goat,
        opposite(F,F1),
        S1=[F1,W,F1,C],
        \+ unsafe(S1).
action([F,W,G,F],S1,Action) :-
        Action=farmer_cabbage,
        opposite(F,F1),
        S1=[F1,W,G,F1],
        \+ unsafe(S1).
action([F,W,G,C],S1,Action) :-
        Action=farmer_alone,
        opposite(F,F1),
        S1=[F1,W,G,C],
        \+ unsafe(S1).

opposite(n,Opp) :- Opp=s.
opposite(s,Opp) :- Opp=n.

unsafe([F,W,G,_C]) :-
        W==G,F\==W.
unsafe([F,_W,G,C]) :-
        G==C,F\==G.
