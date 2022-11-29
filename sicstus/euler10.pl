/*

  Euler Problem 10 in SICStus Prolog

  http://projecteuler.net/index.php?section=problems&id=10
  """
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sictus_prolog/

*/

:- ensure_loaded(hakank_utils).


go :-
        L = [
             % euler10a,
             % euler10b,
             % euler10c,
             % euler10d,
             euler10e % ,
             % euler10f,
             % euler10g
             % euler10h
             % euler10i,
             % euler10j
             ],
        run_problems(L).

%%
%% 1.578s
%%
euler10a :-
        N = 2000000,
        findall(I,
                (between(1,N,I),
                 is_prime2(I)
                 % is_prime3(I)
                 % is_prime(I)                
                 ),
                Primes),
        sum_list(Primes,Sum),
        writeln(Sum).




%%
%% 1.447s
%%
euler10b :-
        N = 2000000,
        prime_sum(N,0,PrimeSum),
        writeln(PrimeSum).

prime_sum(0,Sum,Sum).
prime_sum(N,Sum0,Sum) :-
        (is_prime2(N) ->
            Sum1 is Sum0 + N
        ;
            Sum1 is Sum0
        ),
        N1 is N-1,
        prime_sum(N1,Sum1,Sum).

%%
%% Checking only odd numbers: 4.885s
%%
euler10c :-
        N = 2000000,
        prime_sum_odd(3,N, 2,PrimeSum),
        writeln(PrimeSum).

prime_sum_odd(I,N,Sum,Sum) :- I > N.
prime_sum_odd(I,N,Sum0,Sum) :-
        (
         is_prime2(I)
        ->
         Sum1 #= Sum0 + I
        ;
         Sum1 #= Sum0
        ),
        I1 #= I+2,
        prime_sum_odd(I1,N,Sum1,Sum).

%%
%% 1.578s
%%
euler10d :-
    N = 2000000,
    findall(I,
            (between(3,2,N,I),
             is_prime(I)
            ),
            Primes),
    sum_list(Primes,Sum),
    Sum2 is Sum+2,
    writeln(Sum2).

%%
%% Using next_prime/2: 1.380s
%%
euler10e :-
        N = 2000000,
        prime_sum2(0,N,0,Sum),
        writeln(Sum).

prime_sum2(P,N,Sum,Sum) :- P > N.
prime_sum2(P,N,Sum0,Sum) :-
        next_prime(P,P2),
        Sum1 is Sum0 + P,
        prime_sum2(P2,N,Sum1,Sum).

%%
%% numlist_step/3 and include + is_prime2
%% 1.523s
%%
euler10f :-
        numlist_step(3,2,2000000,L),
        include(is_prime2, L, R),
        sum_list(R,Sum),
        Sum2 #= Sum+2,
        writeln(Sum2).

%%
%% 8.016s
%%
euler10g :-
        primes(2000000,L),
        sum_list(L,Sum),
        writeln(Sum).


%%
%% Using clp: 7.812s
%%
euler10h :-
        primes(2000000,L),
        sum_list(L,Sum),
        writeln(Sum).

%%
%% 1.523s
%%
euler10i :-
  e10i(Sum),
  writeln(Sum).

e10i(Sum) :-
  e10i(3,2,Sum).

e10i(N,S0,S) :-
  N > 2000000,
  S = S0.

e10i(N,S0,S) :-
  N =< 2000000,
  (is_prime2(N) ->
     S1 is S0+N
    ;
     S1 is S0
    ),
  N2 is N+2,
  e10i(N2,S1,S).

%%
%% 1.474s
%%
euler10j :-
    N = 2000000,
    (for(I,3,N),
     fromto(0,In,Out,Sum) do 
     (is_prime(I) ->
         Out is In + I
     ;
         Out is In
     )
    ),
    writeln(Sum).
