/*

  Square root of WONDERFUL in SWI Prolog

  Martin Gardner (June 1961)
  """
  'The Square Root of Wonderful' was the name of a play on Broadway. If
  each letter in WONDERFUL stands for a different digit (zero excluded)
  and if OODDF, using the same code, represent the square root, the what
  _is_ the square root of wonderful?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        FD = [W,O,N,D,E,R,F,U,L],
        FD ins 1..9,

        all_different(FD), 

        WONDERFUL #> 0,
        OODDF #> 0,

        WONDERFUL #= 100000000*W + 10000000*O + 1000000*N + 100000*D + 10000*E + 1000*R +  100*F + 10*U + L,

        OODDF #= 10000*O + 1000*O + 100*D + 10*D + F,

        OODDF*OODDF #= WONDERFUL,

        labeling([],FD),

        writeln(wonderful:WONDERFUL),
        writeln(ooddf:OODDF).