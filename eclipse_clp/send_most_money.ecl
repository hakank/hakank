/*

   SEND + MOST = MONEY in ECLiPSe.

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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).


send_most_money([S,E,N,D,M,O,T,Y], MONEY) :-
        MONEY #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
        ic_global:alldifferent([S,E,N,D,M,O,T,Y]),
        M #\= 0,
        S #\= 0,
        1000 * S + 100 * E + 10 * N + D +
        1000 * M + 100 * O + 10 * S + T #=  MONEY.


go :- 
        %
        % first part: find the maximum value of MONEY
        %
        length(LD, 8),
        LD :: 0..9,
        send_most_money(LD, MONEY),
        % we want to maximize MONEY, but using minimize/2 we negate it.
        MONEY_NEG #= -MONEY,

        writeln("First, find maximum value of MONEY:"),
        minimize(search(LD,0,first_fail,indomain,complete, []), MONEY_NEG),
        writeln([MONEY, LD]),

        %
        % second part: find all solutions for the maximum value of MONEY
        % 
        length(LD2, 8),
        LD2 :: 0..9,
        findall(LD2, 
                (send_most_money(LD2, MONEY),
                labeling(LD2)
                ), AllSolutions),
        length(AllSolutions, Len),
        printf("Here are all (%d) solutions with MONEY = %d:\n", [Len, MONEY]),
        writeln(AllSolutions).
