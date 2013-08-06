/*

  Game theory in B-Prolog.

  2 player zero sum game.

  From Taha, Operations Research (8'th edition), page 528. 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        Game = []([]( 3.0, -1.0, -3.0),
                  [](-2.0,  4.0, -1.0), 
                  [](-5.0, -6.0,  2.0)
               ),

        Rows @= Game^length,
        Cols @= Game[1]^length,

        % the two strategies
        length(X1,Rows),
        lp_domain(X1, 0.0, 1.0),
        length(X2,Cols),
        lp_domain(X2, 0.0, 1.0),

        % value to optimize
        lp_domain(V, -2.0, 2.0),

        % Row player
        foreach(I in 1..Rows, V - sum([X1[J]*Game[J,I] : J in 1..Cols]) $=< 0),
        % sum X1 to 1
        sum(X1) $= 1.0,

        % Column player. Almost the same as for Rows:
        % - use X2 instead of X1
        % - V - Sum >= 0  instead of V - Sum =< 0
        % - Game[I,J] instead of Game[J,I]
        foreach(J in 1..Cols, V - sum([X2[I]* Game[I,J] : I in 1..Rows]) $>= 0),
        % sum X2 to 1
        sum(X2) $= 1.0,

        term_variables([X1,X2], Vars),
        lp_solve([max(V)],Vars),

        writeln(x1:X1),
        writeln(x2:X2),
        writeln([v:V]),
        nl.

