/*

  Place number puzzle in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        findall(X,place_number_puzzle(X),L),
        writeln(L).

place_number_puzzle(X) :-

        N = 8,
        length(X,N),
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

        M @= Graph^length,

        alldifferent(X),

        foreach(I in 1..M, abs(X[Graph[I,1]]-X[Graph[I,2]]) #> 1),

        % symmetry breaking
        X[1] #< X[N],

        labeling(X).
