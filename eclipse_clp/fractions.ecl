/*

  Fractions problem in ECLiPSe.

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/fractions.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/fractions.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_global_gac).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).

selection([input_order,first_fail, anti_first_fail, smallest,largest,
           occurrence,most_constrained,max_regret]).
choice([indomain,indomain_min,indomain_max,indomain_middle,
         indomain_median,indomain_split, indomain_random,
         indomain_interval]).


go :-
        findall([Digits,Backtracks],
                fractions(most_constrained,indomain_median,Digits,Backtracks), 
                List),
        ( foreach([Digits,Backtracks],List) do
              writeln(Digits),
              writeln(backtracks:Backtracks)
        ).

fractions(Selection, Choice, Digits, Backtracks) :-

        Digits = [A,B,C,D,E,F,G,H,I],
        Digits :: 1..9,

        ic:alldifferent(Digits),

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
        search(Vars,0,Selection,Choice,complete, [backtrack(Backtracks)]).

