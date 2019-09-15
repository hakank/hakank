/*

  Euler problem 33 in SWI Prolog

  """
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, 
  is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than 
  one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find 
  the value of the denominator.
  """ 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler33a
            ],
        run_problems(L).

%%
%% 0.0s
%%
euler33a :-
        check(1,9,1,S),
        SInv is 1/S,
        writeln(SInv).
      
check(Y1,Y2,S,S) :- Y1 >= Y2.
check(Y1,Y2,S0,S) :-
        Y1 < Y2,
        check(Y1,Y2,1,9,S0,ZS),
        Y3 is Y1+1,
        check(Y3,Y2,ZS,S).
        
check(_Y1,_Y2,Z1,Z2,S,S) :- Z1 > Z2.
check(Y1,Y2,Z1,Z2,S0,S) :-
        Z1 =< Z2,
        Tmp is 10.0*Y1-Z1,
        Tmp \== 0.0,
        X is 9.0*Y1*Z1/(10.0*Y1-Z1),
        ( (1.0*floor(X)=:=X*1.0, Y1/Z1 < 1.0, X < 10.0 )
        ->
          S1 is (S0*Y1)/Z1
        ;
          S1 is S0
        ),
        Z3 is Z1 + 1,
        check(Y1,Y2,Z3,Z2,S1,S).
