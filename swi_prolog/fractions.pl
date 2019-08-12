/*

  Fractions problem in SWI Prolog

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   fractions(Digits),
   writeln(Digits),
   fail.

go.

fractions(Digits) :-

   Digits = [A,B,C,D,E,F,G,H,I],
   Digits ins 1..9,

   all_different(Digits),

   DD = [D1,D2,D3],
   DD ins 1..81,

   D1 #= 10*B+C,
   D2 #= 10*E+F,
   D3 #= 10*H+I,
   A*D2*D3 + D*D1*D3 + G*D1*D2 #= D1*D2*D3,

   %% symmetry breaking
   A*D2 #>= D*D1,
   D*D3 #>= G*D2,

   %% redundant constraints
   3*A #>= D1,
   3*G #=< D2,

   %% search
   labeling([min],Digits).

