/*

  Euler problem 40 in SICStus Prolog

  """
  An irrational decimal fraction is created by concatenating the positive integers:
   
  0.123456789101112131415161718192021...
   
  It can be seen that the 12th digit of the fractional part is 1.

  If dn represents the nth digit of the fractional part, find the 
  value of the following expression.
  
  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler40a
            ],
        run_problems(L).

%%
%% 0.770s
%%
euler40a :-
        N = 1000000,
        %% Generate all digits for 1..10^6
        findall(S,
                (between(1,N,I),
                 num_to_digit_list(I,S)
                ),
                L),
        flatten(L,Ls),
        findall(T,
                (
                 between(1,6,I),
                 I2 is 10^I,
                 nth1(I2,Ls,T)
                ),
                Ts),
        prodlist(Ts,Prod),
        writeln(Prod).
