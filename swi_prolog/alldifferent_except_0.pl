/*

  Decomposition of global constraint alldifferent_except_0 in SWI Prolog

  From Global constraint catalogue:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  """ 
  Enforce all variables of the collection VARIABLES to take distinct 
  values, except those variables that are assigned to 0.
  
  Example
     (<5, 0, 1, 9, 0, 3>)
  
  The alldifferent_except_0 constraint holds since all the values 
  (that are different from 0) 5, 1, 9 and 3 are distinct.
  """



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% all_different_except_0 and not other constraints. 209 solutions.
%
go :-
        N = 4,
        length(X,N),
        X ins 0..N,
        findall(X,(alldifferent_except_0(X),label(X)),L),
        writeln(L),
        length(L,Len),
        writeln(len=Len),
        nl.

%
% alldifferent_except_0 + increasing. It's 32 solutions.
%
go2 :-
        N = 5,
        length(X,N),
        X ins 0..N,
        findall(X,(alldifferent_except_0(X), increasing(X),label(X)),L),
        writeln(L),
        length(L,Len),
        writeln(len=Len),
        nl.

%
% alldifferent_except_0 and there must be exactly 2 0s.
% There are 5400 solutions.
%
go3 :-
        N = 6,
        length(X,N),
        X ins 0..N,
        findall(X, (alldifferent_except_0(X),count_occurrences(X,0,2),label(X)), L),
        writeln(L),
        length(L,Len),
        writeln(len=Len),
        nl.

%%
%% Note: The implementation of alldifferent_except_0/1 has
%%       moved to hakank_utils.pl
%%

% %%
% %% alldifferent_except_0(X)
% %%
% %% Ensure that all values in Xs which are != 0 are different.
% %%                                %
% alldifferent_except_0(X) :-
%         length(X,Len),
%         findall([I,J], (between(2,Len,I), I1 #= I-1, between(1,I1,J)),L),
%         alldifferent_except_0_(L,X).

% alldifferent_except_0_([], _X).
% alldifferent_except_0_([[I,J]|L],X) :-
%         element(I,X,XI),
%         element(J,X,XJ),        
%         (XI #\= 0 #/\ XJ #\= 0) #==> (XI #\= XJ),
%         alldifferent_except_0_(L,X).

