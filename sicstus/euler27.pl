/*

  Euler problem 27 in SICStus Prolog

  """
  Euler published the remarkable quadratic formula:

  n^2 + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values 
  n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 
  41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which 
  produces 80 primes for the consecutive values n = 0 to 79. The product of the 
  coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

      n^2 + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic 
  expression that produces the maximum number of primes for consecutive 
  values of n, starting with n = 0.
  """ 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).
% :- use_module(library(samsort)).

go :- 
        L = [
             euler27a
            ],
        run_problems(L).

%%
%% 2.290s
%%
euler27a :-
        T is 999,
        TNeg is -T,
        new_mutdict(PrimeCache),
        findall([Len,AB,A,B],
                (between(TNeg,T,A),
                 between(A,T,B),
                 p27(PrimeCache,A,B,Len),
                 AB is A*B
                ),
                L),
        sort(L,LSorted1),
        reverse(LSorted1,LSorted),
        % samsort(@>,L,LSorted), % Slower: 5.209s
        matrix_nth1(LSorted,1,2,MaxValue),
        writeln(MaxValue),
        mutdict_items(PrimeCache,Items),
        writeln(items=Items).
                
%%
%% Testing different of caching:
%% - is_prime3/1 (no cache): 0.834s
%% - is_prime_memo/1 (assertz/1): 0.865s
%% - is_prime_cache/1: (mutdict): 0.941s
%% I.e. no caching is fastest.
%%
p27(PrimeCache,A,B,N) :-
        N0 is 0,
        PP is N0*N0 + A*N0 + B,
        is_prime3(PP),
        % is_prime_memo(PP),
        % is_prime_cache(PrimeCache,PP),
        PP > 1,
        PP < 1000,
        p27_(PrimeCache,PP,A,B,N0,N).

p27_(_PrimeCache,_PP,_A,_B,N,N).
p27_(PrimeCache,PP,A,B,N0,N) :-
        PP < 1000,    
        PP > 1,
        is_prime3(PP), 
        % is_prime_memo(PP), 
        % is_prime_cache(PrimeCache,PP), 
        N1 is N0+1,        
        PP1 is N1*N1 + A*N1 + B,
        p27_(PrimeCache,PP1,A,B,N1,N).


is_prime_cache(PrimeCache,N) :-
    (mutdict_get(PrimeCache,N,Val) ->
        Val == true
    ;
        ( is_prime3(N) ->
            mutdict_put(PrimeCache,N,true)
        ;
            mutdict_put(PrimeCache,N,false),
            false            
        )
    ).

:- dynamic is_prime/2.
is_prime_memo(N) :-
    (is_prime(N,Val) ->
        Val == true
    ;
        (is_prime3(N) ->            
            assertz(is_prime(N,true))
        ;
            assertz(is_prime(N,false)),
            false
        )
    ).
