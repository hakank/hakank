/*

  Fill-a-Pix problem in SICStus Prolog.

  From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  """
  Each puzzle consists of a grid containing clues in various places. The 
  object is to reveal a hidden picture by painting the squares around each 
  clue so that the number of painted squares, including the square with 
  the clue, matches the value of the clue. 
  """
 
  Other names of this puzzle:
 
      * ぬり絵パズル
      * Nurie-Puzzle
      * Majipiku
      * Oekaki-Pix
      * Mosaic
      * Mosaik
      * Mozaïek
      * ArtMosaico
      * Count and Darken
      * Nampre puzzle
      * Komsu Karala!
      * Cuenta Y Sombrea
      * Mosaico
      * Voisimage
      * Magipic
      * Fill-In

 
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  """
  Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
  picture hidden inside. Using logic alone, the solver determines which 
  squares are painted and which should remain empty until the hidden picture 
  is completely exposed.
  """
  
  Fill-a-pix History:
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history


  Compare with the following model:
  * http://www.hakank.org/minizinc/fill_a_pix.pl

  And see the Minesweeper model:
  * http://www.hakank.org/sicstus/minesweeper.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        ( for(P,1,3) do
              format('\nProblem ~w\n',[P]),
              problem(P,Problem),
              fill_a_pix(Problem)
        ).


fill_a_pix(Problem) :-

        
        matrix(Problem,[N,N]),

        matrix(X,[N,N]),
        append(X,XList),
        domain(XList,0,1),

        ( for(I,1,N), 
          param(Problem,X,N) do
              ( for(J,1,N), 
                param(Problem,X,I,N) do
                    matrix_element(Problem,I,J,ProblemIJ),
                    ground(ProblemIJ)
              ->                    
                (
                    % sum the number of neighbors of this cell
                    ( for(A,-1,1),
                      param(X,I,J,N),
                      fromto(0,In,Out,Sum) do
                          ( for(B,-1,1),
                            param(X,I,J,A,N),
                            fromto(0,InB, OutB, BSum) do
                                I+A #>  0, J+B #>  0,
                                I+A #=< N, J+B #=< N
                          -> 
                            ( IA #= I+A, JB #= J+B,
                              matrix_element(X,IA,JB,XIAJB),
                              OutB #= InB + XIAJB  )
                          ; 
                            OutB = InB
                          ),
                          Out #= In + BSum
                    ),
                    Sum #= ProblemIJ % all sums must sum up to Problem[I,J]
                )
              ;
                true
              )        
        ),

        % search
        labeling([min,enum,up], XList),

        pretty_print(X),
        nl,
        fd_statistics.



pretty_print(X) :-
        ( foreach(Row, X) do
          ( foreach(R, Row) do
                    R == 1 
              -> 
                    write('#')
          ;
                    write(' ')
          
          ),
          nl
        ).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



% Puzzle 1 from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% 
problem(1,[[_,_,_,_,_,_,_,_,0,_],
           [_,8,8,_,2,_,0,_,_,_],
           [5,_,8,_,_,_,_,_,_,_],
           [_,_,_,_,_,2,_,_,_,2],
           [1,_,_,_,4,5,6,_,_,_],
           [_,0,_,_,_,7,9,_,_,6],
           [_,_,_,6,_,_,9,_,_,6],
           [_,_,6,6,8,7,8,7,_,5],
           [_,4,_,6,6,6,_,6,_,4],
           [_,_,_,_,_,_,3,_,_,_]]).





% Puzzle 2 from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% 
problem(2, [[0,_,_,_,_,_,3,4,_,3],
            [_,_,_,4,_,_,_,7,_,_],
            [_,_,5,_,2,2,_,4,_,3],
            [4,_,6,6,_,2,_,_,_,_],
            [_,_,_,_,3,3,_,_,3,_],
            [_,_,8,_,_,4,_,_,_,_],
            [_,9,_,7,_,_,_,_,5,_],
            [_,_,_,7,5,_,_,3,3,0],
            [_,_,_,_,_,_,_,_,_,_],
            [4,4,_,_,2,3,3,4,3,_]]).


% Puzzle from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
%
% Code: 030.15x15
% ID: 03090000000
% 
problem(3, [[_,5,_,6,_,_,_,_,_,_,6,_,_,_,_],
            [_,_,7,6,_,4,_,_,4,_,_,8,9,_,5],
            [5,_,_,5,_,5,_,3,_,6,_,7,_,_,6],
            [4,_,2,_,4,_,4,_,3,_,2,_,_,9,_],
            [_,_,_,5,_,4,_,3,_,4,_,4,5,_,6],
            [_,4,3,3,4,_,_,_,4,_,2,_,_,_,_],
            [_,_,_,_,_,_,_,_,_,5,_,_,_,4,_],
            [3,_,3,_,_,3,_,_,_,5,_,4,4,_,_],
            [_,_,_,4,3,_,3,3,_,_,5,7,6,_,_],
            [4,_,_,_,2,_,3,3,2,_,8,9,_,5,_],
            [_,_,3,_,_,_,_,5,_,_,7,_,8,_,_],
            [4,_,_,3,2,_,_,_,_,_,7,_,_,6,_],
            [_,_,4,_,5,4,4,_,_,9,6,_,_,_,_],
            [_,3,5,7,_,6,_,_,_,_,_,_,7,_,_],
            [_,_,4,6,6,_,_,_,6,5,_,_,_,4,_]]).


