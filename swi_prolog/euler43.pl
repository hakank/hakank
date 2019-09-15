/*

  Euler problem 43 in SWI Prolog

  """  
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
  each of the digits 0 to 9 in some order, but it also has a rather interesting 
  sub-string divisibility property.
  
  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
  note the following:
  
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
  
  Find the sum of all 0 to 9 pandigital numbers with this property.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler43a
            ],
        run_problems(L).

%%
%% 0.3s
%%
euler43a :-
        findall(N, e43a(N),L),
        sum_list(L,Sum),
        writeln(Sum).

%%
%% Too slow.
%%
euler43b :-
        numlist(0,9,Is),
        Primes = [2,3,5,7,11,13,17],
        numlist(2,8,Ts),
        findall(L,
                (permutation(Is,L),
                 writeln(L),
                 maplist(e43a_test(L),Ts,Primes)
                ),
                Ls),
        sum_list(Ls,Sum),
        writeln(Sum).

e43a(N) :-
        Primes = [2,3,5,7,11,13,17],
        length(X,10),
        X ins 0..9,

        all_different(X),
        numlist(2,8,Is),
        maplist(e43a_test(X),Is,Primes),
        
        labeling([ffc,bisect],X),
        to_num(X,N).

e43a_test(X,I,P) :-
        element(I,X,XI),
        I1 #= I+1,
        element(I1,X,XI1),
        I2 #= I+2,
        element(I2,X,XI2),
        (100*XI + 10*XI1 + XI2) mod P #= 0.