/*

  Euler Problem 14 in SWI Prolog

  Problem 14
  """
  The following iterative sequence is defined for the set of positive integers:

  n n/2 (n is even)
  n 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following 
  sequence:
  13 40 20 10 5 16 8 4 2 1

  It can be seen that this sequence (starting at 13 and finishing at 1) 
  contains 
  10 terms. Although it has not been proved yet (Collatz Problem), it is 
  thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.)
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/


:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             euler14a %% ,
             %% euler14b
            ],
        run_problems(L).


%%
%% 10.2s 
%%
euler14a :-
        abolish_all_tables,
        findall(Len-N,
                (
                 between(3,2,999_999,N),
                 collLength(N,Len)
                ),
                L),
        %% sort(1, @>, L, Sorted),
        %% [(MaxLen-N)|_] = Sorted,
        max_member(Max,L),
        (MaxLen-N) = Max,
        writeln([n=N,len=MaxLen]).

%%
%% 18.2s
%%
euler14b :-
        abolish_all_tables,
        e14b(3,999_999,[1,0],Max),
        writeln(Max).

e14b(N,Limit,Max,Max) :- N > Limit. % , !.
e14b(N,Limit,[M,Max0],Max) :-
        N =< Limit,
        collLength(N,Len),
        (Len > Max0
        ->
         Max1 = [N,Len]
        ;
         Max1 = [M,Max0]
         ),
        N1 is N + 2,
        e14b(N1,Limit,Max1,Max).
        

:- table collLength/2.
collLength(1,1). % :- !. % slightly faster with a cut.
collLength(N,L) :-
        N > 1,
        (
         N mod 2 =:= 0
        ->
         T is N div 2
        ;
         T is 3*N+1
        ),
        collLength(T,L1),
        L is L1 + 1.


%%
%% Experimental using mode tabling.
%% After http://picat-lang.org/euler/p14.pi
%%
%% But SWI-Prolog version 8.1.13 don't give the correct answer...
%% It shoule be fixed in 8.1.14.
%%
%% This takes about 34s.
%%
%% ?- make,abolish_all_tables,time(euler14xxx).
%%
%% [n=3,len=525]
%% chain_len=8
%% 
euler14xxx :-
        writeln("Nope, it got the correct length (525) but not the correct N (837799) nor the correct Chain."),
        % halt,
        abolish_all_tables,
        max_chain(N,Chain,Len),
        writeln([n=N,len=Len]),
        length(Chain,ChainLen),
        writeln(chain_len=ChainLen).

:- table max_chain(-,-,max).
max_chain(N,Chain,Len) :-
        %% between(3,2,999999,N),  % checking the odd numbers
        between(2,999999,N),  % checking all numbers -> [n=2,len=525], chain_len=2
        % writeln(n=N),
        gen(N,Chain,Len).

:- table gen(+,-,-). % original
gen(1,Chain,Len) :-
        !,
        Chain=[1],
        Len=1.
gen(N,Chain,Len) :-
        N > 1,
        N mod 2 =:= 0, % !,
        Ndiv2 is N div 2,
        gen(Ndiv2,Chain1,Len1),
        Chain=[N|Chain1],
        Len is Len1+1.
gen(N,Chain,Len) :-
        N > 1,
        N mod 2 =:= 1, % !,
        T3N1 is 3*N+1,
        gen(T3N1,Chain1,Len1),
        Chain=[N|Chain1],
        Len is Len1+1.
