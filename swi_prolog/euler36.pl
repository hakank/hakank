/*

  Euler problem 36 in SWI Prolog

  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
  in both bases.
  
  Find the sum of all numbers, less than one million, which are palindromic 
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler36a
            ],
        run_problems(L).

%%
%% 0.55s
%%
euler36a :-
        findall(N,
                (between(1,999_999,N),
                 palindromic2(N),
                 dec_to_base_list(N,2,L2),
                 palindromic(L2)
                ),
                L),
        sum_list(L,Sum),
        writeln(Sum).
        
%%
%% dec_to_base_list(N,Base,L)
%%
%% Convert decimal integer N to a list L of digits in base Base.
%%
dec_to_base_list(N,Base,L) :-
        dec_to_base_list(N,Base,[],L).

dec_to_base_list(0,_Base,L,L).
dec_to_base_list(N,Base,L0,[R|L]) :-
        N > 0,
        R is N mod Base,
        N1 is N div Base,
        dec_to_base_list(N1,Base,L0,L).
