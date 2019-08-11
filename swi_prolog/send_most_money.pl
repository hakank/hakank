/*

  SEND+MOST=MONEY problem in SWI Prolog

  Alphametic problem were we want to maximize MONEY.

  This version do two things:
    - find the maximum of MONEY
    - and then find all solutions for the maximum value of MONEY.

  Problem from the lecture notes:
  http://www.ict.kth.se/courses/ID2204/notes/L01.pdf


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        %
        % first part: find the maximum value of MONEY
        %
        writeln("First part: find the maximum value of MONEY"),
        length(LD,8),
        LD ins 0..9,
        send_most_money(LD, MONEY),
        once(labeling([max(MONEY)], LD)),
        writeln(max=MONEY),

        %
        % second part: find all solutions for the maximum value of MONEY
        %
        writeln("Second part: find all solutions with max value of MONEY"),
        length(LD2, 8),
        LD2 ins 0..9,
        findall(LD2, (send_most_money(LD2, MONEY),label(LD2)), AllSolutions),
 
        length(AllSolutions, Len),
        writeln(AllSolutions),
        writeln(len=Len),
        nl.

send_most_money([S,E,N,D,M,O,T,Y], MONEY) :-

        all_different([S,E,N,D,M,O,T,Y]),
        S #> 0,
        M #> 0,
        MONEY #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
        1000*S + 100*E + 10*N + D +
        1000*M + 100*O + 10*S + T #= MONEY.
