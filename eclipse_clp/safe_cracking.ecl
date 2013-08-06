/*
  
  Safe cracking problem in ECLiPSe.

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

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:-lib(ic).

go :-
        findall(LD, safe(LD), L),
        writeln(L)
        .

safe(LD) :-
        % LD=[C1, C2, C3, C4, C5, C6, C7, C8, C9], % list version
        LD=[](C1, C2, C3, C4, _C5, C6, C7, C8, C9), % array version
        LD :: 1..9,
        alldifferent(LD),
         
        % C1 <> 1, C2 <> 2, ..., C9 <> 9
        (for(I,1,9), param(LD) do
             LD[I] #\= I

             % for the list version:
             % listut:nth1(I,LD,N),
             % I #\= N
        ),

        C4 - C6 #= C7,
        C1 * C2 * C3 #= C8 + C9,
        C2 + C3 + C6 #< C8,
        C9 #< C8,

        search(LD, 0, first_fail, indomain, complete, []).
