/*

  Mrs Timpkin's Age problem in ECLiPSe.

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.
 
  When the Timpkinses married eighteen years ago, Timpkins was three
  times as old as his wife, and today he is just twice as old as she.
  How old is Mrs. Timpkin? 

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

go :-
        findall(LD, timpkin(LD),L),
        writeln(L).


timpkin(LD) :-
        
        LD = [T, W],
        LD :: 1..100,
        T - 18 #= 3 * (W - 18),
        T #= 2 * W,

        labeling(LD).


