/*

  Euler problem 30 in SWI Prolog

  """
  Surprisingly there are only three numbers that can be written 
  as the sum of fourth powers of their digits:

     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

  As 1 = 1^(4) is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of 
  fifth powers of their digits.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler30a
             %% euler30b
            ],
        run_problems(L).

%%
%% 1.12s
%%
euler30a :-
        From is 10,
        To is 6*9^5,
        findall(N,
                (between(From,To,N),
                 num_to_digit_list(N,Is),
                 maplist(sum_pow5,Is,Ps),
                 sum_list(Ps,N)
                ),
                L),
        sum_list(L,Sum),
        writeln(Sum).

%%
%% 1.2s
%%
euler30b :-
        From is 10,
        To is 6*9^5,
        e30b(From,To,0,S),
        writeln(S).

e30b(N,N,S,S).
e30b(N,To,S0,S) :-
        num_to_digit_list(N,Is),
        maplist(sum_pow5,Is,Ps),
        (
         sum_list(Ps,N)
        ->
         S1 is S0 + N
        ;
         S1 is S0
        ),
        N1 is N+1,
        e30b(N1,To,S1,S).

sum_pow5(I,P) :- P is I^5.
