/*

  Mama's age problem in SICStus Prolog.

  Mamma's Age from "Amusements in Mathematics, Dudeney", number 40.
  """
  Tommy: "How old are you, mamma?"
  Mamma: "Our three ages add up to exactly seventy years."
  Tommy: "And how old are you, papa?"
  Papa: "Just six times as old as you, my son."
  Tommy: "Shall I ever be half as old as you, papa?"
  Papa: "Yes, Tommy; and when that happens our three ages will add up to
  exactly twice as much as today."

  Can you find the age of Mamma?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/mamas_age.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/mamas_age.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go:-
     findall([mama:M,papa:P,tommy:T],mama([M,P,T]),L), 
     write(L),nl.


mama(LD) :-
        LD = [M,P,T],
        domain(LD,0,500),
        M + P + T #= 70 * 12,
        6 * T #= P,
        (T + I) * 2 #= P + I,
        M + I + P + I + T + I #= 2 * (M + P + T),
        
        labeling([], LD).
