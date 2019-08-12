/*

  Seesaw problem in SWI Prolog

  Marriott & Stuckey "Programming with Constraints", page 257.
 
  Balancing on a seesaw.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

% direct modeling
go :-

        LS = [Liz,Fi,Sara],
        LS ins -5..5,

        9 * Liz + 8 * Fi + 4 * Sara #= 0,
        apart(Liz, Fi, 3),
        apart(Liz, Sara, 3),
        apart(Sara, Fi, 3),

        %% symmetry breaking
        Sara #>= 0,
        labeling([], LS),

        writeln([liz=Liz,fi=Fi,sara=Sara]),
        nl.

%%
%% Page 258 suggests cumulative instead of apart.
%%
go2 :-
        LS = [Liz,Fi,Sara],
        LS  ins -5..5,

        9 * Liz + 8 * Fi + 4 * Sara #= 0,
        %% symmetry breaking
        Sara #>= 0,

        
        %% LD = [3,3,3],
        %%LR = [1,1,1],
        %%Limit = 1,
        Tasks = [task(Liz, 3, _, 1, 1),
                 task(Fi, 3, _, 1, 2),
                 task(Sara, 3, _, 1, 3)
                ],

        cumulative(Tasks, [limit(1)]),

        label(LS),

        write([liz=Liz,fi=Fi,sara=Sara]),
        nl,nl.


apart(X, Y, N) :-
   (X #>= Y + N) #\/ (Y #>= X + N).

