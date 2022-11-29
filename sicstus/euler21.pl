/*

  Euler problem 21 in SICStus Prolog

  """
  Let d(n) be defined as the sum of proper divisors of n (numbers less 
  than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable 
  pair and each of a and b are called amicable numbers.
  
  For example, the proper divisors of 220 are 
  1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
  The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
  
  Evaluate the sum of all the amicable numbers under 10000.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).


go :-
        L = [
             euler21a % ,
             % euler21b
             ],
        run_problems(L).

%%
%% 0.061s
%%
euler21a :-
        N = 10000,
        findall(A,
                (
                 between(1,N, A),
                 amicable(A)
                ),
                As),
        sum_list(As,Sum),
        writeln(Sum).

%%
%% 12.191s
%%
euler21b :-
        N = 10000,
        findall(A,
                (
                 between(2,N, A),
                 amicable2(A)
                ),
                As),
        sum_list(As,Sum),
        writeln(Sum).




amicable(A) :-
        A > 1,
        sum_proper_divisors(A,B),
        A \= B,        
        sum_proper_divisors(B,C),
        A =:= C.


amicable2(A) :-
        A > 1,
        sum_proper_divisors2(A,B),
        A \= B,        
        sum_proper_divisors2(B,C),
        A =:= C, !.

