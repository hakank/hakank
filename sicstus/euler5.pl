/*

  Euler Problem 5 in SICStus Prolog

  http://projecteuler.net/index.php?section=problems&id=5
  """
  2520 is the smallest number that can be divided by each of the 
  numbers from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of 
  the numbers from 1 to 20?
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :-
        L = [
            % euler5a,
            euler5b
            ],
        run_problems(L).

% 0.000s
euler5a :-
        numlist(2,20,Ls),
        foldr(lcm, 1, Ls, Res),
        writeln(Res).

% alternative with do-loops: 0.000s
euler5b :-
        ( for(I,2,20),
          fromto(1,In,Out,Lcm) do
          lcm(I,In,Out)
        ),
        writeln(Lcm).
