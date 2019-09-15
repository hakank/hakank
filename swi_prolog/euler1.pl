/*

  Euler Problem 1 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=1
  """
  If we list all the natural numbers below 10 that are multiples of 3 
  or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             % euler1a,
             euler1b % ,
             % euler1c,
             % euler1d,
             % euler1e
            ],
        run_problems(L).

%%
%% 0.119s
%%
euler1a :-
        euler1a_pred(1, 999, 0, Res),
        writeln(Res).

%%
%% 0.001s
%%
euler1b :-
        numlist(1,999,Is),
        maplist(p,Is,Ls),
        sum_list(Ls,Res),
        writeln(Res).

%%
%% 0.006s
%%
euler1c :-
        setof(I,
                (between(1,999,I),
                 (I mod 3 #= 0 ; I mod 5 #= 0)
                ),
                Is
               ),
        sum_list(Is,Res),
        writeln(Res).

%%
%% 0.001s
%%
euler1d :-
        numlist(1,999,Is),
        include(p_d,Is,Ls),
        sum_list(Ls,Res),
        writeln(Res).

%%
%% 0.001s
%%
euler1e :-
        numlist(1,999,Is),
        setof(I,
                (member(I,Is),
                 p_d(I)
                ),
                Ls
               ),
        sum_list(Ls,Res),
        writeln(Res).


euler1a_pred(N, Limit, R, R) :- N #> Limit.
euler1a_pred(N, Limit, R, R2) :-
        B in 0..1,
        (N mod 3 #= 0 #\/N mod 5 #= 0) #<==> B #= 1,
        R1 #= R + B*N,
        N2 #= N + 1,
        euler1a_pred(N2, Limit, R1,R2).


p(N,N) :- N mod 3 #= 0.
p(N,N) :- N mod 5 #= 0.
p(_,0).


p_d(N) :- N mod 3 #= 0 ; N mod 5 #= 0.