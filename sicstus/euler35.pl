/*

  Euler problem 35  in SICStus Prolog

  """
  The number, 197, is called a circular prime because all rotations 
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler35a
            ],
        run_problems(L).

%%
%% 1.163s
%%
euler35a :-
        findall(N,
                (between(1,1000000,N),
                 is_prime(N),
                 circular_prime(N)
                ),
                L),
        length(L,Len),
        writeln(Len).

circular_prime(N) :-
        num_to_digit_list(N,Is),
        length(Is,Len),
        circular_prime(1,Len, Is).
circular_prime(Len,Len,_Is).
circular_prime(Len1,Len,Is) :-
        Len1 =< Len,
        rotate(Is,Is2),
        % rotate_list(1,Is,Is2), % Not faster
        to_num(Is2,Num2),
        is_prime(Num2),
        Len2 is Len1 + 1,
        circular_prime(Len2,Len,Is2).


