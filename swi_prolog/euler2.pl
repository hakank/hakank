/*

  Euler Problem 2 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=2
  """
  Each new term in the Fibonacci sequence is generated by adding the previous two 
  terms. By starting with 1 and 2, the first 10 terms will be:

  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

  Find the sum of all the even-valued terms in the sequence which do not exceed 
  four million.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             euler2a % ,
             %% euler2b
            ],
        run_problems(L),
        nl.

%%
%% 0.001s
%%
euler2a :-
        abolish_all_tables,
        p2(Total), % get all < 4000000
        include(even,Total,L),
        sum_list(L,Sum),
        writeln(Sum).

%%
%% 0.001s
%%
euler2b :-
        abolish_all_tables,
        numlist(1,100,I), % The 100th Fib is large enough...
        maplist(fib,I,Fibs),
        include(less_than_and_even(4000000),Fibs,L),
        sum_list(L,Sum),
        writeln(Sum).



% This generates a list of all fib numbers < 4000000
p2(Total) :-
        p2(1, 1, Total).

% Some trickery to remove last element.
p2(C, _, [F|Total]) :-
        fib(C, F),
        (F #< 4000000
        ->
         C1 #= C+1,
         p2(C1, F, Total)
        ;
         Total = []
        ).

less_than_and_even(Y,X) :- X < Y, even(X).