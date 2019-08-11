/*

  Safe cracking problem in SWI Prolog

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The code of Professor Smart's safe is a sequence of 9 distinct 
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

 
   C1 <> 1, C2 <> 2, ..., C9 <> 9

  can you find the correct combination?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        safe(LD),
        writeln(LD),
        nl.

%% Check all solutions
go2 :-
        findall(LD, safe(LD),L),
        writeln(L).

safe(LD) :-
   N = 9,
   LD=[C1, C2, C3, C4, _C5, C6, C7, C8, C9],
   LD ins 1..9,

   all_different(LD),
    
   % C1 <> 1, C2 <> 2, ..., C9 <> 9
   numlist(1,N,Is),
   maplist(not_ix(LD), Is),
   
   C4 - C6 #= C7,
   C1 * C2 * C3 #= C8 + C9,
   C2 + C3 + C6 #< C8,
   C9 #< C8,

   labeling([], LD).

% C1 <> 1, C2 <> 2, ..., C9 <> 9
not_ix(LD, I) :-
        element(I,LD,E),
        E #\= I.