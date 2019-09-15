/*

  Euler problem 37 in SWI Prolog

  """
  The number 3797 has an interesting property. Being prime itself, it is possible to 
  continuously remove digits from left to right, and remain prime at each stage: 
  3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right 
  and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler37a
            ],
        run_problems(L).

%%
%% 7.0s
%%
euler37a :-
        abolish_all_tables,
        %% 2, 3, 5, and 7 are not considered truncable primes
        %% so we start on 9
        N = 11, % the start number
        S0 = 0, % the sum
        C0 = 0, % the counter
        C1 = 11, % there are 11 truntable primes
        e37a(N,C0,C1,S0,S),
        writeln(S).

e37a(_N,C,C,S,S).
e37a(N,C0,C,S0,S) :-
        C0 =< C,
        (
         truncatable_prime(N)
        ->
         C1 #= C0 + 1,
         S1 #= S0 + N
        ;
         C1 #= C0,
         S1 #= S0
        )
        ,
        %% next_prime(N,N2), % slower
        N2 #= N+2, % faster
        e37a(N2,C1,C,S1,S).


truncatable_prime(N) :-
        prime_tabled(N),
        num_to_digit_list(N,L),

        findall(X,append(X,Y,L),Left1),
        delete(Left1,[],Left),
        check_prime_list(Left),

        findall(Y,append(X,Y,L),Right1),
        delete(Right1,[],Right),
        check_prime_list(Right).


check_prime_list([]).
check_prime_list([X|Xs]) :-
        digit_list_to_num(X,10,N),
        prime_tabled(N),
        check_prime_list(Xs).
