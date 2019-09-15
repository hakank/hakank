/*

  Euler Problem 4 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=4
  """
  A palindromic number reads the same both ways. The largest palindrome
  made from the product of two 2-digit numbers is 9009 = 91*99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             euler4a % ,
             %% euler4b             
            ],
        run_problems(L).



% 0.31s
euler4a :-
        From = 100,
        To = 999,        
        findall(IJ,
                (between(From,To,I),
                 between(I,To,J),
                 IJ #= I*J,
                 palindromic2(IJ)
                ),
                IJs),
        max_list(IJs,Max),
        writeln(Max).

%%
%% Skipping findall/3 and between/3, and rolling
%% a double loop manually.
%% Slightly slower: 0.43s
%%
euler4b :-
        From = 100,
        To = 999,
        e4b(From,From,From,To,0,Max),
        writeln(Max).

e4b(To,_FromBase,To,To,Max,Max).
e4b(From,FromBase,To0,To,Max0,Max) :-
        From #=< To0,
        T #= To0*From,
        (
         (T #> Max0, palindromic2(T))
        ->
         Max1 #= T
        ;
         Max1 #= Max0
        ),
        From1 #= From + 1,
        e4b(From1,FromBase,To0,To,Max1,Max).
% Reset From
e4b(From,FromBase,To0,To,Max0,Max) :-
        From #> To0,
        To1 #= To0 + 1,
        e4b(FromBase,FromBase,To1,To,Max0,Max).
