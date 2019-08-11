/*

  Place number puzzle in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
    findall(X,place_number_puzzle(X),L),
    writeln(L),
    nl.

place_number_puzzle(X) :-

        graph(Graph),
        N #= 8,
        length(X,N),
        X ins 1..N,
        
        all_distinct(X),

        %% Ensure that two neighbours are not consecutive.
        maplist(constraint(X),Graph),

        %% symmetry breaking
        element(1,X,X1),
        element(N,X,XN),
        X1 #< XN,
        
        labeling([ffc,enum],X).


graph(Graph) :-
        Graph = 
        [[1,2], [1,3], [1,4],
         [2,1], [2,3], [2,5], [2,6],
         [3,2], [3,4], [3,6], [3,7],
         [4,1], [4,3], [4,6], [4,7],
         [5,2], [5,3], [5,6], [5,8],
         [6,2], [6,3], [6,4], [6,5], [6,7], [6,8],
         [7,3], [7,4], [7,6], [7,8],
         [8,5], [8,6], [8,7]].
              
% abs(X[Graph[I,1]]-X[Graph[I,2]) #> 1
constraint(X,Node) :-
        element(1,Node,E1),
        element(E1,X,X1),
        
        element(2,Node,E2),
        element(E2,X,X2),
        
        abs(X1-X2) #> 1.
