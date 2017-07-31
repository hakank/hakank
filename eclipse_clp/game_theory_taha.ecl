/*


  Game theory in ECLiPSe.

  2 player zero sum game.

  From Taha, Operations Research (8'th edition), page 528. 


  Compare with the following Comet model:
  http://www.hakank.org/comet/game_theory_taha.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(eplex).
:-lib(listut).


go :-
        eplex_solver_setup(max(V)),

        Game = []([]( 3.0, -1.0, -3.0),
                  [](-2.0,  4.0, -1.0), 
                  [](-5.0, -6.0,  2.0)
               ),

        dim(Game,[Rows,Cols]),

        % the two strategies
        length(X1,Rows),
        X1 :: 0.0..1.0,

        length(X2,Cols),
        X2 :: 0.0..1.0,


        % value to maximize
        V :: -2.0..2.0,
        
        % 
        % Row player
        %
        ( for(I,1,Rows), 
          param(X1,Game,Cols,V) do
              ( for(J,1,Cols), 
                fromto(0,In,Out,Sum),
                param(Game,X1,I) do
                    nth1(J,X1,X1J),
                    Out = In + (X1J*Game[J,I])
              ),
              V - eval(Sum) $=< 0
        ),

        % sum X1 to 1
        1.0 $= sum(X1),

        
        % 
        % Column player. Almost the same as for Rows:
        % * use X2 instead of X1
        % * V - Sum >= 0  instead of V - Sum =< 0
        % * Game[I,J] instead of Game[J,I]
        %
        ( for(I,1,Cols), 
          param(X2,Game,Rows,V) do
              ( for(J,1,Rows), 
                fromto(0,In,Out,Sum),
                param(Game,X2,I) do
                    nth1(J,X2,X2J),
                    Out = In + (X2J*Game[I,J])
              ),
              V - eval(Sum) $>= 0
        ),

        % sum X2 to 1
        1.0 $= sum(X2),

       
        eplex_solve(V),
        eplex_get(vars,SolveVars),
        eplex_get(typed_solution, SolveVals),
        SolveVars = SolveVals,

        writeln(x1:X1),
        writeln(x2:X2),
        writeln(v:V).

