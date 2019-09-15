/*

  Euler problem 41 in SWI Prolog

  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             %% euler41a,
             euler41b
            ],
        run_problems(L).

%%
%% 5.06s
%%
euler41a :-
        findall(P,
                (
                 between(2,9,N),
                 numlist(1,N,L),
                 permutation(L,Perm),
                 digit_list_to_num(Perm,P),
                 is_prime(P)
                ),
                L),
        max_list(L,Max),
        writeln(Max).
        
%%
%% 5.05s
%%
euler41b :-
        
        findall(P,
                (
                 between(2,9,N),
                 numlist(1,N,L),
                 permutation(L,Perm),
                 digit_list_to_num(Perm,P),
                 is_prime(P),
                 nb_setval(p,P)
                ),
                L),
        nb_getval(p,Max),
        writeln(Max).
        



%%
%% Extremly slow (what a surprise! :-)
%%
euler41xxx :-
        findall(P,
                (between(3,2,987654321,P),
                 num_to_digit_list(P,Ps),
                 all_different(Ps),
                 is_prime(P)
                ),
                L),
        length(L,Len),
        writeln(Len),
        max_list(L,Max),
        writeln(Max).
