/*

  Place number puzzle in ECLiPSe.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  """

  Compare with these models:
  * MiniZinc: http://www.hakank.org/minizinc/place_number.mzn
  * Comet   : http://www.hakank.org/comet/place_number_puzzle.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).


go :-
        findall(X,place_number_puzzle(X),L),
        writeln(L).

place_number_puzzle(X) :-

        N = 8,
        dim(X,[N]),
        X :: 1..N,

        Graph = 
        []([](1,2),
           [](1,3),
           [](1,4),
           [](2,1),
           [](2,3),
           [](2,5),
           [](2,6),
           [](3,2),
           [](3,4),
           [](3,6),
           [](3,7),
           [](4,1),
           [](4,3),
           [](4,6),
           [](4,7),
           [](5,2),
           [](5,3),
           [](5,6),
           [](5,8),
           [](6,2),
           [](6,3),
           [](6,4),
           [](6,5),
           [](6,7),
           [](6,8),
           [](7,3),
           [](7,4),
           [](7,6),
           [](7,8),
           [](8,5),
           [](8,6),
           [](8,7)),

        dim(Graph,[M,2]),

        alldifferent(X),

        ( for(I,1,M),
          param(Graph,X) do
              abs(X[Graph[I,1]]-X[Graph[I,2]]) #> 1
        ),

        % symmetry breaking
        X[1] #< X[N],

        labeling(X).
