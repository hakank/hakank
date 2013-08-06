/*

  SEND+MOST=MONEY problem in B-Prolog.

  Alphametic problem were we maximize MONEY.

  This version do two things:
    - find the maximum of MONEY
    - and then find all solutions for the maximum value of MONEY.

  Problem from the lecture notes:
  http://www.ict.kth.se/courses/ID2204/notes/L01.pdf

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :- 
        %
        % first part: find the maximum value of MONEY
        %
        length(LD, 8),
        LD :: 0..9,
        send_most_money(LD, MONEY),

        writeln('First, find maximum value of MONEY:'),
        maxof(labeling(LD),MONEY),
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
        format('\nHere are all (~d) solutions with MONEY = ~d:\n', [Len, MONEY]),
        foreach(Solution in AllSolutions, writeln(Solution)),
        nl.


send_most_money([S,E,N,D,M,O,T,Y], MONEY) :-
        alldifferent([S,E,N,D,M,O,T,Y]),
        S #> 0,
        M #> 0,
        MONEY #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
        1000*S + 100*E + 10*N + D +
        1000*M + 100*O + 10*S + T #= MONEY.


