/*

  Euler problem 48 in SWI Prolog

  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler48a
            ],
        run_problems(L).

%%
%% 0.005s
%%
euler48a :-
        findall(J,
                (
                between(1,1000,I),
                 J is I^I
                ),
                L
               ),
        sum_list(L,Sum1),
        atom_chars(Sum1,Sum2),
        reverse(Sum2,Sum),
        findall(S,
                (
                 between(1,10,I),
                 nth1(I,Sum,S)
                ),
                L2),
        reverse(L2,L3),
        atom_chars(Sol,L3),
        writeln(Sol).

        


