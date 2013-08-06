/*

  Hidato puzzle in SICStus Prolog.

  http://www.shockwave.com/gamelanding/hidato.jsp
  http://www.hidato.com/
 
  """
  Puzzles start semi-filled with numbered tiles.
  The first and last numbers are circled.
  Connect the numbers together to win. Consecutive
  number must touch horizontally, vertically, or
  diagonally.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/hidato.mzn
  * Comet : http://www.hakank.org/comet/hidato.co
  * Gecode: www.hakank.org/gecode/hidato.cpp
  * ECLiPSe: www.hakank.org/eclipse/hidato.ecl
  * Tailor/Essence': http://www.hakank.org/tailor/hidato.eprime

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        NumProblems = 6,
        ( for(P,1,NumProblems) do
              % findall(_,solve(P),_)
              solve(P)
        ).

% See the comments for problem 7 below
go2 :-
        problem(7,Problem),
        findall(Problem,hidato(Problem),List),
        ( foreach(L,List),
          foreach(P,Positions) do
              write(L),nl,
              append(L,LL),
              element(P,LL,16)
        ),
        write(positions:Positions),nl,
        length(Positions,PositionsLen),
        sort(Positions,UniquePositions),
        write(unique_positions:UniquePositions),nl,
        length(UniquePositions,UniqueLen),
        format('The number 16 can be in ~d unique positions (total ~d solutions)\n',[UniqueLen,PositionsLen]),
        fd_statistics.


% generate all solutions for a given grid size
go3 :-
        N = 3,
        matrix(X,[N,N]),      
        findall(_,hidato(X),L),
        length(L,Len),
        format('\nIt was ~d solutions\n',[Len]).
       


solve(Problem) :-
        format('\nProblem ~d\n', Problem),
        problem(Problem,X),
        hidato(X).


hidato(X) :-

        length(X, N),
        append(X, XList),
        NSquared is N*N,
        domain(XList, 1,NSquared),

        %
        % place all integers from 1..n*n
        %
        all_different(XList),
       
        NN1 is (N*N)-1,
        ( for(K,1,NN1),
            param(N,XList) do
        
              % define temporal variables for finding
              % the index of this (K) and the next number (K+1)
              domain([I,J],1,N),  % index I, J
              domain([A,B],-1,1), % offset from K's position
              
              IA #= I+A,
              JB #= J+B,

              IA #>= 1,
              JB #>= 1,
              IA #=< N,
              JB #=< N,
              abs(A)+abs(B) #>= 1, % both A and B cannot be 0, i.e. it
                                   % must be a move

              % 1) First: fix this K
              IJ #= (I-1)*N + J,
              element(IJ, XList, K),
              
             
              % 2) then, find the position of the next value
              IA_JB #= (I-1+A)*N + J+B,
              K1 is K+1,
              element(IA_JB, XList, K1)

        ),

        labeling([ff,bisect,down], XList),

        pretty_print(X), 
        fd_statistics.



pretty_print(X) :-
        ( foreach(Row,X) do
              write(Row), nl
        ),nl.        


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


%
% Problems
%


% Simple problem
%
% solution:
%   6 7 9
%   5 2 8
%   1 4 3
% 
problem(1, [[6,_,9],
            [_,2,8],
            [1,_,_]]).



problem(2, [[ _,44,41, _, _, _, _],
            [ _,43, _,28,29, _, _],
            [ _, 1, _, _, _,33, _],
            [ _, 2,25, 4,34, _,36],
            [49,16, _,23, _, _, _],
            [ _,19, _, _,12, 7, _],
            [ _, _, _,14, _, _, _]]). 



% Problems from the book:
% Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

% problem 1 (Practice)
problem(3, [[_, _,20, _, _],
            [_, _, _,16,18],
            [22, _,15, _, _],
            [23, _, 1,14,11],
            [_,25, _, _,12]]).
              


% problem 2 (Practice)
problem(4, [[_, _, _, _,14],
            [_,18,12, _, _],
            [_, _,17, 4, 5],
            [_, _, 7, _, _],
            [9, 8,25, 1, _]]).


% problem 3 (Beginner)
problem(5, [[ _,26, _, _, _,18],
            [ _, _,27, _, _,19],
            [31,23, _, _,14, _],
            [ _,33, 8, _,15, 1],
            [ _, _, _, 5, _, _],
            [35,36, _,10, _, _]]).



% Problem 15 (Intermediate)
problem(6,[[64, _, _, _, _, _, _, _],
           [ 1,63, _,59,15,57,53, _],
           [ _, 4, _,14, _, _, _, _],
           [ 3, _,11, _,20,19, _,50],
           [ _, _, _, _,22, _,48,40],
           [ 9, _, _,32,23, _, _,41],
           [27, _, _, _,36, _,46, _],
           [28,30, _,35, _, _, _, _]]).


% From Scott
% Kim's column in November 2008 issue of Discover Magazine, 
% http://discover.coverleaf.com/discovermagazine/200811/?pg=78#pg78
% Via
% http://www.scottblogs.com/missed-solution-to-discover-hidato-puzzle/
% (where I found the problem).
% The problem is stated in this picture:
% http://www.scottblogs.com/images/hidato-1.jpg
% """
% There are three numbers given in the 4-by-4 Hidato puzzle.
% This puzzle is not completely set up, though. How many 
% possible locations are there for the final number, 16,
% that leads to a unique solution path?
% """
% The solution page at
%   http://discover.coverleaf.com/discovermagazine/200811/?pg=78#pg80
% states that there are 4 solutions. This is not correct, as we will
% see.
%
% The predicate go2/0 checks this with the following output:
%
% [[16,15,14,4],[12,13,3,5],[11,2,9,6],[1,10,7,8]]
% [[16,14,13,4],[15,12,3,5],[11,2,9,6],[1,10,7,8]]
% [[16,13,14,4],[12,15,3,5],[11,2,9,6],[1,10,7,8]]
% [[15,14,13,4],[16,12,3,5],[11,2,9,6],[1,10,7,8]]
% [[14,15,16,4],[13,12,3,5],[11,2,9,6],[1,10,7,8]]
% [[14,15,16,4],[12,13,3,5],[11,2,9,6],[1,10,7,8]]
% [[14,13,16,4],[12,15,3,5],[11,2,9,6],[1,10,7,8]]
% [[13,16,15,4],[12,14,3,5],[11,2,9,6],[1,10,7,8]]
% [[13,15,16,4],[14,12,3,5],[11,2,9,6],[1,10,7,8]]
% [[13,15,16,4],[12,14,3,5],[11,2,9,6],[1,10,7,8]]
% [[13,14,16,4],[12,15,3,5],[11,2,9,6],[1,10,7,8]]
% [[13,14,15,4],[12,16,3,5],[11,2,9,6],[1,10,7,8]]
% [[13,12,11,4],[14,10,3,5],[15,2,9,6],[1,16,7,8]]
% [[11,12,13,4],[10,14,3,5],[9,2,15,6],[1,8,7,16]]
% positions:[1,1,1,5,3,3,3,2,3,3,3,6,14,16]
% unique_positions:[1,2,3,5,6,14,16]
% The number 16 can be in 7 unique positions (total 14 solutions)
%
problem(7,      
        [[_,_,_,4],
         [_,_,_,_],
         [_,_,_,_],
         [1,_,7,_]]).            