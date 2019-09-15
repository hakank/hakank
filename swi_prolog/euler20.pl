/*

  Euler problem 20 in SWI Prolog

  """
  n! means n (n 1) ... 3 2 1
  
  Find the sum of the digits in the number 100!"
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler20a %%,
             %% euler20b,
             %% euler20c,
             %% euler20d
            ],
        run_problems(L).

%%
%% 0.000s
%%
euler20a :-
        factorial2(100,F),
        digits_sum(F,Sum),        
        writeln(Sum).

%%
%% 0.000s
%%
euler20b :-
        factorial3(100,F),
        digits_sum(F,Sum),        
        writeln(Sum).

%%
%% 0.00s
%%
euler20c :-
        factorial4(100,F),
        digits_sum(F,Sum),        
        writeln(Sum).


%%
%% 0.002s
%%
euler20d :-
        n_factorial(100,F), % clpfd version
        digits_sum(F,Sum),
        writeln(Sum).
