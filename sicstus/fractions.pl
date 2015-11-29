/*

  Fractions problem in SICStus Prolog.

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(Digits,fractions(Digits), List),
        ( foreach(L,List) do
              write(L),nl
        ),
        fd_statistics.

fractions(Digits) :-

        Digits = [A,B,C,D,E,F,G,H,I],
        domain(Digits,1,9),

        DD = [D1,D2,D3],
        domain(DD,1,81),

        all_distinct(Digits),

        D1 #= 10*B+C,
        D2 #= 10*E+F,
        D3 #= 10*H+I,
        A*D2*D3 + D*D1*D3 + G*D1*D2 #= D1*D2*D3,

        % break the symmetry
        A*D2 #>= D*D1,
        D*D3 #>= G*D2,

        %redundant constraints
        3*A #>= D1,
        3*G #=< D2,

        % search
        append(Digits,DD,Vars),
        labeling([min,step,up], Vars).


        

 

