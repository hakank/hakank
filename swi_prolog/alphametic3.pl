/*

  General alphametic (cryptarithmetic) solver in SWI Prolog

  This version is a port of the ECLiPSe CLP model by Joachim Schimpf:
  http://eclipseclp.org/examples/cryptarith.ecl.txt
  """
  Examples:
  %
  % ?- cryptarith([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y], 10, Sol).
  % 
  % ?- cryptarith([D,O,N,A,L,D] + [G,E,R,A,L,D] = [R,O,B,E,R,T], 10, Sol).
  % 
  % ?- cryptarith([S,I,X] + [S,I,X] + [S,I,X] + [B,E,A,S,T] = [S,A,T,A,N], 10, Sol).
  % 
  % By Jim Gillogly, rec.puzzles 2003-09-05 (base 12, 2 solutions):
  % ?- cryptarith([[K,K,K] = [6,6,6],
  %                [K,K,K] = [G,E,O,R,G,E] - [W,A,L,K,E,R] + [B,U,S,H]], 12, Sol).
  """

  It's mainly in expression/3 that I've changed compared with the ECLiPSe model.
  (I also added the Sol parameter in the cryptarith/3 predicate.)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% SEND+MORE=MONEY
%%
go :-
        cryptarith([S,E,N,D]+[M,O,R,E]=[M,O,N,E,Y],10,Sol),
        writeln([S,E,N,D]+[M,O,R,E]=[M,O,N,E,Y]),
        writeln(sol=Sol),
        nl.

%%
%% DONALD+GERALD=ROBERT
%%
go2 :-
        cryptarith([D,O,N,A,L,D] + [G,E,R,A,L,D] = [R,O,B,E,R,T],10,Sol),
        writeln([D,O,N,A,L,D] + [G,E,R,A,L,D] = [R,O,B,E,R,T]),
        writeln(sol=Sol),
        nl.

%%
%% SEND+MANY+MORE=MONEY has 6 solutions
%% 
go3 :-
        cryptarith([S,E,N,D]+[M,A,N,Y]+[M,O,R,E]=[M,O,N,E,Y],10,Sol),
        writeln([S,E,N,D]+[M,A,N,Y]+[M,O,R,E]=[M,O,N,E,Y]),
        writeln(sol=Sol),
        nl.

%%
%%  SIX + SIX + SIX + BEAST = SATAN
%%
%% and
%%
%%  3*SIX + BEAST = SATAN
%%
go4 :-
        cryptarith([S,I,X] + [S,I,X] + [S,I,X] + [B,E,A,S,T] = [S,A,T,A,N], 10, Sol),
        writeln([S,I,X] + [S,I,X] + [S,I,X] + [B,E,A,S,T] = [S,A,T,A,N]),
        writeln(Sol).

go4b :-
        cryptarith([3]*[S,I,X] + [B,E,A,S,T] = [S,A,T,A,N], 10, Sol),
        writeln([3]*[S,I,X] + [B,E,A,S,T] = [S,A,T,A,N]),
        writeln(Sol).
        


%%
%% Interactive version: Write an expression (as a Prolog term, i.e. end with ".")
%% Note: We assume base 10.
%%
%% Examples:
%%   [S,E,N,D]+[M,O,R,E]=[M,O,N,E,Y].
%%   [D,O,N,A,L,D] + [G,E,R,A,L,D] = [R,O,B,E,R,T].
%%   [S,I,X] + [S,I,X] + [S,I,X] + [B,E,A,S,T] = [S,A,T,A,N].
%%   [S,E,N,D]+[M,A,N,Y]+[M,O,R,E]=[M,O,N,E,Y].
%% 
go5 :-
        writeln("Write a Prolog term to solve. Eg: [S,E,N,D]+[M,O,R,E]=[M,O,N,E,Y]."),
        read_term(Ls,[]),
        cryptarith(Ls,10,Sol),
        writeln(Ls),
        writeln(Sol),
        nl.


cryptarith(Equations, Base,Digits) :-
	term_variables(Equations, Digits),
        Base1 #= Base-1,
	Digits ins 0..Base1,
	all_different(Digits),
	constraint(Equations, Base),
	labeling([ff],Digits).

constraint([], _).
constraint([C|Cs], Base) :-
	constraint(C, Base),
	constraint(Cs, Base).

constraint(E1=E2, Base) :-
	expression(E1, CE1, Base),
	expression(E2, CE2, Base),
	CE1 #= CE2.
	
expression(E1+E2, CE1+CE2, Base) :-
	expression(E1, CE1, Base),
	expression(E2, CE2, Base).

expression(E1-E2, CE1-CE2, Base) :-
	expression(E1, CE1, Base),
	expression(E2, CE2, Base).

expression(E1*E2, CE1*CE2, Base) :-
	expression(E1, CE1, Base),
	expression(E2, CE2, Base).
        

expression(Digits, WeightedDigits, Base) :-
	Digits = [First|_],
	First #\= 0,
        to_num(Digits,Base,WeightedDigits).
