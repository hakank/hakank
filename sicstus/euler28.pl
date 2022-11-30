/*

  Euler problem 28 in SWI Prolog

  """
  Starting with the number 1 and moving to the right in a clockwise 
  direction a 5 by 5 spiral is formed as follows:
  
     21 22 23 24 25
     20  7  8  9 10
     19  6  1  2 11
     18  5  4  3 12
     17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.
  
  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler28a % ,
             % euler28b,
             % euler28c
            ],
        run_problems(L).

%%
%% 0.001s
%%
euler28a :-
        S0 is 1,
        N is 3,
        e28a(N,S0,S),
        writeln(S).

e28a(N,S,S) :- N > 1001.
e28a(N,S0,S) :-
        S1 is S0 + 4 * N^2 - 6 * N + 6,
        N1 is N + 2,
        e28a(N1,S1,S).

%%
%% 0.009s
%%
euler28b :-
        findall(S,
                (between(3,1001,N),
                 N mod 2 =:= 1,
                 S is 4 * N^2 - 6 * N + 6
                ),
                L),
        sum_list(L,Sum0),
        Sum is Sum0 + 1,
        writeln(Sum).

%%
%% 0.001s
%%
euler28c :-
        ( for(N,3,1001),
          fromto(1,In,Out,Sum) do
          (N mod 2 =:= 0 -> 
              Out is In + 4*N*N - 6*N + 6
          ;
              Out is In
          )
        ),
        writeln(Sum).
