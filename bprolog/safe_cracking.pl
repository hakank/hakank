/*

  Safe cracking problem in B-Prolog.

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
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        safe(LD),
        writeln(LD),nl.

go2 :-
        findall(LD, safe(LD), L),
        write(L),nl.

safe(LD) :-
        N = 9,
        LD=[C1, C2, C3, C4, _C5, C6, C7, C8, C9],
        LD :: 1..9,

        alldistinct(LD),
         
        % C1 <> 1, C2 <> 2, ..., C9 <> 9
        foreach((I,L) in (1..N,LD), L #\= I),
        
        C4 - C6 #= C7,
        C1 * C2 * C3 #= C8 + C9,
        C2 + C3 + C6 #< C8,
        C9 #< C8,

        labeling([forward,split], LD).
