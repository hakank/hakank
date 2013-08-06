/*

  Five brigands problem in SICStus Prolog.

  From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  The Five Brigands    from "Amusements in Mathematics, Dudeney",
  number 133.

  The five Spanish brigands, Alfonso, Benito, Carlos, Diego, and Esteban,
  were counting their spoils after a raid, when it was found that they
  had captured altogether exacly 200 doubloons. One of the band pointed
  out that if Alfonso had twelve times as much, Benito three times as
  much, Carlos the same amount, Diego half as much, and Esteban one-
  third as much, they would still have altogether just 200 doubloons.
  How many doubloons had each?

  There are a good many equally correct answers to this problem. The
  puzzle is to discover exactly how many different answers there are, it
  being understood that every man had something and there is to be no
  fractional money. 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/five_brigades.mzn
  * ECLiPSe: http://www.hakank.org/eclipse/five_brigands.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(LD, brigands(LD), L),
        length(L, Len),
        write(len:Len),nl,
        (
            foreach(El, L) do
                write(El),nl
        ).

brigands(LD) :-
        LD = [A,B,C,D2,E3],
        domain(LD,8,160), % Everybody has at least 8d.; nobody has more than 160
        A + B + C + 2*D2 + 3*E3 #= 200,
        A * 12 + B * 3 + C + D2 + E3  #= 200,
        D2 in 8..100,
        E3 in 8..66,

        labeling([],LD).
