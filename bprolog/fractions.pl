/*

  Fractions problem in B-Prolog.

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        fractions(Digits),
        writeln(Digits),
        fail.

fractions(Digits) :-

        Digits = [A,B,C,D,E,F,G,H,I],
        Digits :: 1..9,

        alldifferent(Digits),

        DD = [D1,D2,D3],
        DD :: 1..81,

        D1 #= B*C,
        D2 #= E*F,
        D3 #= H*I,
        A*D2*D3 + D*D1*D3 + G*D1*D2 #= D1*D2*D3,

        % break the symmetry
        A*D2 #>= D*D1,
        D*D3 #>= G*D2,

        %redundant constraints
        3*A #>= D1,
        3*G #=< D2,


        % search
        term_variables([Digits],Vars),
        labeling([ffc,split],Vars).

