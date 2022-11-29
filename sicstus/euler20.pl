/*

  Euler problem 20 in SICStus Prolog

  """
  n! means n (n 1) ... 3 2 1
  
  Find the sum of the digits in the number 100!"
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

% Defines the postfix version
:- op(600, xf, '!').
'!'(N, Factorial) :- factorial2(N,Factorial).

go :- 
        L = [
             euler20a % ,
             % euler20b,
             % euler20c,
             % euler20d,
             % euler20e
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
%% 0.000s
%%
euler20d :-
        n_factorial(100,F), 
        digits_sum(F,Sum),
        writeln(Sum).

%%
%% 0.000s
%%
euler20e :-
        !(100,F),
        digits_sum(F,Sum),        
        writeln(Sum).
