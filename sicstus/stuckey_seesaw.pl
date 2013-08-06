/*

  Seesway problem in SICStus Prolog.

  Marriott & Stuckey "Programming with Constraints", page 257.
 
  Balancing on a seesaw.

  Comparing with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/stuckey_seesaw.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

% direct modeling
go :-
        Liz  in -5..5,
        Fi   in -5..5,
        Sara in -5..5,

        9 * Liz + 8 * Fi + 4 * Sara #= 0,
        apart(Liz, Fi, 3),
        apart(Liz, Sara, 3),
        apart(Sara, Fi, 3),

        % symmetry breaking
        Sara #>= 0,

        labeling([], [Liz,Fi,Sara]),

        write([liz:Liz,fi:Fi,sara:Sara]),nl,nl,
        % fail,
        fd_statistics.

%
% Page 258 suggests cumulative instead of apart.
%
go2 :-
        Liz  in -5..5,
        Fi   in -5..5,
        Sara in -5..5,

        9 * Liz + 8 * Fi + 4 * Sara #= 0,

        % symmetry breaking
        Sara #>= 0,

        LS = [Liz,Fi,Sara],
        LD = [3,3,3],
        LR = [1,1,1],
        Limit = 1,
        
        % cumulative([Liz, Fi, Sara], [3,3,3], [1,1,1], 1),
        my_cumulative([Liz, Fi, Sara], LD, LR, Limit, LE),

        append(LS,LE,Vars1),
        labeling([], Vars1),

        write([liz:Liz,fi:Fi,sara:Sara]),nl,nl,
        % fail,
        fd_statistics.


apart(X, Y, N) :- X #>= Y + N #\/ Y #>= X + N.


%
% cumulative is more complex in SICStus nowadays...
%
my_cumulative(LS,LD,LR,Limit,LE) :-

        % setup a list of end times LE
        ( foreach(S1,LS),
          foreach(D1,LD),
          foreach(K,LE) do
              K #= S1+D1
        ),
        ( foreach(S, LS),
          foreach(D, LD),
          foreach(E, LE),
          foreach(R, LR),
          foreach(task(S,D,E,R,0),Tasks) do
              true
        ),
        cumulative(Tasks, [limit(Limit)]).

