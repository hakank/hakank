/*

  Global constraint alldifferent_modulo in SWI Prolog

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_modulo.html
  """
  Enforce all variables of the collection VARIABLES to have a distinct 
  rest when divided by M.
  
  Example
  (<25, 1, 14, 3>, 5)
  
  The equivalence classes associated with values 25, 1, 14 and 3 are 
  respectively equal to 
     25 mod 5 = 0, 1 mod 5 = 1, 14 mod 5 = 4 and 3 mod = 3. 
  Since they are distinct the alldifferent_modulo constraint holds.
  """

  alldifferent_modulo/2 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   N = 4,
   length(X,N),
   X ins 1..20,

   M in 2..5,
   indomain(M),
   writeln(m=M),
   % M = 5,

   findall([mod=M,X], (alldifferent_modulo(X, M), flatten([X,M], Vars), labeling([],Vars)),L),
   length(L,Len),

   maplist(writeln,L),
   writeln(len=Len).

go.

%%
%% Reverse problem: find M given list X
%%
go2 :-
   N = 4,
   length(X,N),
   X ins 1..20,

   X = [20,19,13,6],
   
   M in 2..5,

   findall([mod=M,X], (alldifferent_modulo(X, M), flatten([X,M], Vars), labeling([],Vars)),L),
   length(L,Len),

   maplist(writeln,L),
   writeln(len=Len).
