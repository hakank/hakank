/*

  Farmer planning problem in SWI Prolog

  See https://en.wikipedia.org/wiki/River_crossing_puzzle
  
  This is a port of the B-Prolog/Picat model farmer.pl/farmer.pi.
 
  There are two possible shortest plans of length 7:

  farmer_goat
  farmer_alone
  farmer_wolf
  farmer_goat
  farmer_cabbage
  farmer_alone
  farmer_goat


  farmer_goat
  farmer_alone
  farmer_cabbage
  farmer_goat
  farmer_wolf
  farmer_alone
  farmer_goat
  
  The bplan module is here: http.//hakank.org/swi_prolog/bplan.pl
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(bplan).

go :-
        abolish_all_tables,
        bplan(Plan),
        maplist(writeln,Plan),
        length(Plan,Len),
        writeln(len=Len),
        nl.
        
initial_state([s,s,s,s]).

goal_state([n,n,n,n]).

legal_move([F,F,G,C],Action,S1) :-
        Action=farmer_wolf,
        opposite(F,F1),
        S1=[F1,F1,G,C],
        \+ unsafe(S1).
legal_move([F,W,F,C],Action,S1) :-
        Action=farmer_goat,
        opposite(F,F1),
        S1=[F1,W,F1,C],
        \+ unsafe(S1).
legal_move([F,W,G,F],Action,S1) :-
        Action=farmer_cabbage,
        opposite(F,F1),
        S1=[F1,W,G,F1],
        \+ unsafe(S1).
legal_move([F,W,G,C],Action,S1) :-
        Action=farmer_alone,
        opposite(F,F1),
        S1=[F1,W,G,C],
        \+ unsafe(S1).

opposite(n,Opp) :- Opp=s.
opposite(s,Opp) :- Opp=n.

unsafe([F,W,G,_C]) :- W == G,F \== W.
unsafe([F,_W,G,C]) :- G == C, F \== G.
