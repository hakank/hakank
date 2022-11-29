/*

  Euler Problem 14 in SICStus Prolog

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
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/


:- ensure_loaded(hakank_utils).

go :-
        L = [
             euler14a % ,
             % euler14b
             % euler14c,
             % euler14d
             ],
        run_problems(L).


%%
%% 1.689s
%%
euler14a :-
        findall(Len-N,
                (
                 between(3,2,999999,N),
                 collLength(N,Len)
                ),
                L),
        %% sort(1, @>, L, Sorted),
        %% [(MaxLen-N)|_] = Sorted,
        max_member(Max,L),
        (_MaxLen-N) = Max,
        writeln(N).

%%
%% 4.737s
%%
euler14b :-
    e14b(3,999999,[1,0],Max),
    Max = [M,_Max0],
    writeln(M).

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
%% 3.527s
%%
euler14c :-
    N = 1000000,
    ( for(I,1,N),
      fromto(Lens,Out,In,[]) do
      collatz_list(I,List),
      length(List,Len),
      Out = [Len|In]
    ),
    max_member(Max,Lens),
    nth1(Nth,Lens,Max),!,
    writeln(Nth).


collatz(1,1) :- !.
collatz(N,C) :-
        N #> 1,
        N mod 2 =:= 0, !,
        C is N // 2.
collatz(N,C) :-
        N > 1,
        N mod 2 \= 0, !,
        C is 1+3*N.

collatz_list(1,[1]) :- !.
collatz_list(N,[N|Res]) :-
        collatz(N,C),
        collatz_list(C,Res).

%%
%% Using a cache: 3.185s
%%
euler14d :-
        new_mutdict(CollatzHash),
        N = 1000000,
        ( for(I,1,N),
          fromto(Lens,Out,In,[]),
          param(CollatzHash) do
              collatz_list2(I, Len, CollatzHash), 
              Out = [Len|In]
        ),
        max_member(Max,Lens),
        nth1(Nth,Lens,Max),!,
        writeln(Nth).

collatz_list2(1,1,_) :- !.
collatz_list2(N, Len2, CollatzHash) :-
        N > 1,
        (
        mutdict_get(CollatzHash, N, Val) ->
            Len2 is Val-1
        ;
            collatz(N,C),
            % collatz_list2(C, Len, CollatzHash),
            collatz_list(C,L),
            length(L,Len),
            Len2 is Len + 1,
            mutdict_put(CollatzHash, C, Len2)
        ).
