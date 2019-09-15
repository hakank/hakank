/*

  Euler problem 34 in SWI Prolog

  Problem 34
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler34a
            ],
        run_problems(L).

%%
%% 0.3s
%%
euler34a :-
        abolish_all_tables,
        findall(N,
               (between(10,100_000,N),
                num_to_digit_list(N,Is),
                maplist(factorial_cached,Is,Fs),
                sum_list(Fs,N)
               ),
               L),
        sum_list(L,Sum),
        writeln(Sum).

:- table factorial_cached/2.
factorial_cached(N,F) :-
        factorial4(N,F).
        