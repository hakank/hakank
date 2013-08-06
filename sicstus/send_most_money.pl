/*

   SEND + MOST = MONEY in SICStus Prolog.

   Alphametic problem were we maximize MONEY.

   This version do two things:
    - find the maximum of MONEY
    - then show all solutions for the maximum value of MONEY

   Problem from the lecture notes:
   http://www.ict.kth.se/courses/ID2204/notes/L01.pdf


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/send_most_money.mzn
              only finds one solution
  * Comet   : http://www.hakank.org/comet/send_most_money2.co
              finds all solutions
  * Gecode/R: http://www.hakank.org/gecode_r/send_most_money2.rb
              finds all solutions
  * Tailor/Essence': http://www.hakank.org/tailor/send_most_money.eprime
              find one solution
  * ECLiPSe : http://www.hakank.org/eclipse/send_most_money.ecl
              finds all solutions

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :- 
        %
        % first part: find the maximum value of MONEY
        %
        length(LD, 8),
        domain(LD, 0, 9),
        send_most_money(LD, MONEY),

        write('First, find maximum value of MONEY:'),nl,
        labeling([leftmost,maximize(MONEY)],LD),
        write([MONEY, LD]),nl,
        % fd_statistics,


        %
        % second part: find all solutions for the maximum value of MONEY
        %
        length(LD2, 8),
        domain(LD2, 0, 9),
        findall(LD2, 
                (send_most_money(LD2, MONEY),
                labeling([leftmost], LD2)
                ), AllSolutions),
        length(AllSolutions, Len),
        format('Here are all ~d solutions with MONEY = ~d:\n', [Len, MONEY]),
        write(AllSolutions),nl,
        fd_statistics.


send_most_money([S,E,N,D,M,O,T,Y], MONEY) :-
        all_different([S,E,N,D,M,O,T,Y]),
        MONEY #= 10000*M + 1000*O + 100*N + 10*E + Y,
        M #\= 0,
        S #\= 0,
        1000 * S + 100 * E + 10 * N + D +
        1000 * M + 100 * O + 10 * S + T #=  MONEY.

