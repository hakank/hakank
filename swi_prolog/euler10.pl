/*

  Euler Problem 10 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=10
  """
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).


go :-
        %%
        %% All euler10a..10f takes about the same 
        %% time (~19s) since is_prime/1 etc are
        %% are too slow...
        %% euler10g use primes/2 which is a kind of
        %% prime sieve (kind of).
        %%
        L = [
             %euler10a,
             %euler10b,
             %euler10c,
             %euler10d,
             %euler10e,
             %%euler10f,
             euler10g
             ],
        run_problems(L).

%%
%% 18.5s
%%
euler10a :-
        N = 2000000,
        findall(I,
                (between(1,N,I),
                 is_prime2(I)
                 % is_prime(I)                
                 ),
                Primes),
        sum_list(Primes,Sum),
        writeln(Sum).




%%
%% 20.1s
%%
euler10b :-
        N = 2000000,
        prime_sum(N,0,PrimeSum),
        writeln(PrimeSum).

prime_sum(0,Sum,Sum).
prime_sum(N,Sum0,Sum) :-
        (
         is_prime(N)
        ->
         Sum1 is Sum0 + N
        ;
         Sum1 is Sum0
        ),
        N1 is N-1,
        prime_sum(N1,Sum1,Sum).

%%
%% Checking only odd numbers: 19.2s
%%
euler10c :-
        N = 2000000,
        prime_sum_odd(3,N, 2,PrimeSum),
        writeln(PrimeSum).

prime_sum_odd(I,N,Sum,Sum) :- I > N.
prime_sum_odd(I,N,Sum0,Sum) :-
        (
         is_prime(I)
        ->
         Sum1 #= Sum0 + I
        ;
         Sum1 #= Sum0
        ),
        I1 #= I+2,
        prime_sum_odd(I1,N,Sum1,Sum).

%%
%% 19.413s
%%
euler10d :-
        N = 2000000,
        findall(I,
                (between(3,2,N,I),
                is_prime(I)
                ),
                Primes),
        sum_list(Primes,Sum),
        Sum2 #= Sum+2,
        writeln(Sum2).

%%
%% Using next_prime/2: 19.372s
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
%% 19.1s
%%
euler10f :-
        numlist_step(3,2,2000000,L),
        include(is_prime2, L, R),
        sum_list(R,Sum),
        Sum2 #= Sum+2,
        writeln(Sum2).

%%
%% 8.4s
%%
euler10g :-
        %% We must increase the stack for this...
        set_prolog_stack(global, limit(10_000_000_000)),
        primes(2_000_000,L),
        sum_list(L,Sum),
        writeln(Sum).


%%
%% Using clp: 11.4s
%%
euler10h :-
        %% We must increase the stack for this...
        set_prolog_stack(global, limit(10_000_000_000)),
        primes2(2_000_000,L),
        sum_list(L,Sum),
        writeln(Sum).



primes2(N,L) :-
        N2 #= 1+(N div 2),
        findall(J2,(between(2,N2,I),
                    II #= I*I,
                    II #=< N,
                    NDivI #= 1+(N div I),
                    between(0,NDivI,J),
                    J2 #= II+I*J,
                    J2 #=< N
                   ),
                Js),
        sort(Js,Deletes),        
        numlist(2,N,All),
        delete_all(All,Deletes,L).

