/*

  Seesway problem in ECLiPSe.

  Marriott & Stuckey "Programming with Constraints", page 257.
 
  Balancing on a seesaw.

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/stuckey_seesaw.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/stuckey_seesaw.pl



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_cumulative).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).



% direct modeling, using apart
go :-
        Liz  :: -5..5,
        Fi   :: -5..5,
        Sara :: -5..5,

        9 * Liz + 8 * Fi + 4 * Sara #= 0,
        apart(Liz, Fi, 3),
        apart(Liz, Sara, 3),
        apart(Sara, Fi, 3),

        % symmetry breaking
        Sara #>= 0,

        labeling([Liz,Fi,Sara]),

        write([liz:Liz,fi:Fi,sara:Sara]),nl,
        fail.

% Page 258 suggests cumulative instead of apart.
go2 :-
        Liz  :: -5..5,
        Fi   :: -5..5,
        Sara :: -5..5,

        9 * Liz + 8 * Fi + 4 * Sara #= 0,

        % symmetry breaking
        Sara #>= 0,

        LS = [Liz,Fi,Sara],
        LD = [3,3,3],
        LR = [1,1,1],
        Limit = 1,
        cumulative(LS, LD, LR, Limit),

        term_variables([LS],Vars1),
        labeling(Vars1),

        write([liz:Liz,fi:Fi,sara:Sara]),nl,fail.


apart(X, Y, N) :- 
        X #>= Y + N or Y #>= X + N.
