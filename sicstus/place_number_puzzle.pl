/*

  Place number puzzle in SICStus Prolog.

 
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
  

  Two solutions (plus their reverses):
  x = [2, 5, 8, 6, 3, 1, 4, 7]
  
  x = [2, 6, 8, 5, 4, 1, 3, 7]


  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/place_number.mzn
  * Comet   : http://www.hakank.org/comet/place_number_puzzle.co
  * ECLiPSe : http://www.hakank.org/eclipse/place_number_puzzle.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(_,place_number(_X),_),
        fd_statistics.


place_number(X) :-

        graph(Graph),

        N = 8,
        length(X,N),
        domain(X,1,N),

        all_different(X),

        ( foreach([From,To],Graph),
          param(X) do
              element(From,X,XFrom),
              element(To,X,XTo),
              abs(XFrom-XTo) #> 1
        ),

        % symmetry breaking
        element(1,X,X1),
        element(N,X,XN),
        X1 #< XN,

        labeling([], X),
        write(X),nl.


graph([[1,2],
       [1,3],
       [1,4],
       [2,1],
       [2,3],
       [2,5],
       [2,6],
       [3,2],
       [3,4],
       [3,6],
       [3,7],
       [4,1],
       [4,3],
       [4,6],
       [4,7],
       [5,2],
       [5,3],
       [5,6],
       [5,8],
       [6,2],
       [6,3],
       [6,4],
       [6,5],
       [6,7],
       [6,8],
       [7,3],
       [7,4],
       [7,6],
       [7,8],
       [8,5],
       [8,6],
       [8,7]]).
   
