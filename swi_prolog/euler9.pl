/*

  Euler Problem 9 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c,
  for which, a^2 + b^2 = c^2.

  For example, 32 + 42 = 9 + 16 = 25 = 52.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product a*b*c.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).


go :-
        L = [euler9a %%,
             %% euler9b
            ],
        run_problems(L).


%% 0.086s
euler9a :- 
        triplet(_L, Prod),
        writeln(Prod) .

%%
%% 0.203s
%%
euler9b :-
        N = 1000,
        N2 #= N//2,
        between(1,N2,C),
        N3 #= (N-C//2)-C,
        between(-1,N3,B),
        A is N - B - C,
        A > 0,
        A **2 + B**2 =:= C**2,
        (
        is_pyth(A,B,C)
        ->
         ABC is A*B*C,
         writeln(ABC)
        ).

%% using CLP(FD)
triplet([A, B, C], Prod) :-
     LD = [A,B,C],
     LD ins 1..500,
     A + B + C #= 1000,
     A #=< B, % symmetry breaking
     B #=< C, 
     Prod #= A * B * C,
     A^2 + B^2 - C^2 #= 0,
     labeling([ffc,bisect], LD).
        

is_pyth(A,B,C) :- A**2+B**2 =:= C**2.