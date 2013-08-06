/*

  Strimko puzzles in B-Prolog.

  From 
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain 
  the number 1, 2, ... n exactly once (that is, the grid must form a 
  Latin square), and each "stream" (connected path in the grid) must 
  also contain the numbers 1, 2, ..., n exactly once.
  """
 
  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        strimko(2).

go2 :-
        strimko(2),
        strimko(67),
        strimko(68),
        strimko(69),
        strimko(70).


strimko(P) :-
        format('\nProblem ~w\n',[P]),
        problem(P,N,Streams,Placed),

        new_array(X,[N,N]),
        array_to_list(X,XList),
        XList :: 1..N,

        % is a latin square
        alldifferent_matrix(X),

        % streams
        foreach(S in 1..N,
                [Positions],
                (Positions @= [X[I,J] : I in 1..N, J in 1..N, S =:= Streams[I,J]],
                 alldifferent(Positions)
                )
               ),
        

        % placed
        foreach(I in 1..Placed^length, 
                (X[ Placed[I,1], Placed[I,2] ] #= Placed[I,3])),

        labeling(XList),
        pretty_print(X),
        nl.


pretty_print(X) :-
        N @= X^length,
        foreach(I in 1..N,
                (
                    foreach(J in 1..N, format(' ~d ', [X[I,J]])),
                    nl
                )),
        nl.



%
% Ensure a Latin square, 
% i.e. all rows and all columns are different
%
alldifferent_matrix(Board) :-
        Rows @= Board^rows,
        foreach(Row in Rows, alldifferent(Row)),
        Columns @= Board^columns,
        foreach(Column in Columns, alldifferent(Column)).


%
% Strimko Monthly #02
% Via http://www.hakank.org/minizinc/strimko2_002.dzn
%
problem(2,7,Streams, Placed) :-
    Streams = [[1,1,2,2,2,2,2],
               [1,1,2,3,3,3,2],
               [1,4,1,3,3,5,5],
               [4,4,3,1,3,5,5],
               [4,6,6,6,7,7,5],
               [6,4,6,4,5,5,7],
               [6,6,4,7,7,7,7]],
    Placed =  [[2,1,1],
               [2,3,7],
               [2,5,6],
               [2,7,4],
               [3,2,7],
               [3,6,1],
               [4,1,4],
               [4,7,5],
               [5,2,2],
               [5,6,6]].

% 
% Strimko Weekly Set 067
% Via http://www.hakank.org/minizinc/strimko2_067.dzn
problem(67,5,Streams, Placed) :-
 Streams = [[1,1,1,2,3],
             [1,2,2,2,3],
             [1,2,4,5,3],
             [5,4,5,4,3],
             [4,5,5,4,3
             ]],
 Placed = [[1,3,4],
           [1,4,1],
           [3,3,2],
           [3,5,3],
           [5,4,5]].

%
% Strimko Weekly Set 068
% Via http://www.hakank.org/minizinc/strimko2_068.dzn
problem(68,4,Streams,Placed) :-

Streams = [[1,2,2,4],
           [2,1,4,2],
           [3,4,1,3],
           [4,3,3,1]],
Placed =    [[2,2,3],
             [2,3,2],
             [3,3,1]].


% Strimko Weekly Set 069
% Via http://www.hakank.org/minizinc/strimko2_069.dzn
problem(69,6,Streams,Placed) :-
   Streams = [[1,2,3,3,3,4],
              [2,1,3,5,4,3],
              [2,1,3,5,5,4],
              [2,6,1,6,5,4],
              [2,6,1,6,4,5],
              [6,2,6,1,5,4]],
   Placed =  [[2,2,4],
              [2,3,1],
              [2,4,3],
              [2,5,2],
              [3,2,1],
              [3,5,6],
              [4,3,5],
              [4,4,2]].

% Strimko Weekly Set 070
% Via http://www.hakank.org/minizinc/strimko2_070.dzn
problem(70,5,Streams,Placed) :-
Streams =  [[1,2,3,3,3],
            [2,1,1,3,1],
            [2,2,3,1,4],
            [5,2,5,4,4],
            [5,5,5,4,4]],
Placed =   [[1,1,1],
            [2,5,4],
            [4,1,2],
            [5,4,5]].
