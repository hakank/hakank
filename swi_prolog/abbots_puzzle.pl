/*

  Abbot's puzzle in SWI Prolog

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.

  If 100 bushels of corn were distributed among 100 people in such a
  manner that each man received three bushels, each woman two, and each
  child half a bushel, how many men, women, and children were there?

  Dudeney added the condition that there are five times as many women as
  men. That way, the solution becomes unique (otherwise, there are seven
  solutions). 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   findall(LD, abbot(LD),L),
   writeln(L),
   nl.

abbot(LD) :-
   LD = [M, W, C],
   LD ins 1..100,
   M + W + C #= 100,

   % Men: 3, Women: 2, Children: 1/2 = 100
   M * 3 + W * 2 + C//2 #= 100,
   M * 5 #= W,    % additional condition added by Dudeney      

   label(LD).


