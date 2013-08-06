/*

  Hidato puzzle in ECLiPSe.
  
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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(listut).


go :-
        NumProblems = 6,
        ( for(P,1,NumProblems) do
              solve(P)
        ).


solve(Problem) :-
        printf("\nProblem %d\n", [Problem]),
        problem(Problem,X),
        hidato(X).


hidato(X) :-

        % decision variables
        dim(X,[N,N]),
        X :: 1..N*N,

        flatten_array(X, XList), % as list

        %
        % place all integers from 1..r*c
        %
        ic_global:alldifferent(XList),
       
        % XList = [6,7,9,5,2,8,1,4,3], % solution problem 1

        % more declarative version (based on my Gecode version)
        % this version has fewer failures and choices(but more
        % propagations)
        NN1 is (N*N)-1,
        ( for(K,1,NN1),
          param(N,XList) do

              % define temporal variables for finding
              % the index of this and the next number (K)
              I :: 1..N, % index I
              J :: 1..N, % index J
              A :: -1..1, % offset from K's position
              B :: -1..1, % ibid
              
              % needed for nth1
              IA #= I+A,
              JB #= J+B,

              % some extra constraints
              IA #>= 1,
              JB #>= 1,
              IA #=< N,
              JB #=< N,
              abs(A)+abs(B) #>= 1, % both A and B cannot be 0, i.e. it
                                   % must be a move
              % 1) First: fix this k, i.e.
              % K #= X[I,J], % don't work (instantiation fault)
              IJ #= (I-1)*N + J,
              nth1(IJ, XList, K),
              
             
              % 2) Then, find the position of the next value, i.e.
              % K+1 #= X[I+A,J+B], % don't work
              IA_JB #= (I-1+A)*N + J+B,
              K1 is K+1,
              nth1(IA_JB, XList, K1)

        ),

        term_variables(X,Vars),
        search(Vars,0,smallest,indomain_maxn,complete,[backtrack(Backtracks)]),

        pretty_print(X), 
        writeln(backtracks:Backtracks).



pretty_print(X) :-
        dim(X, [N,N]),
        ( for(I, 1, N), param(X, N) do
            ( for(J, 1, N), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        ),nl.        



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
problem(1, []([](6,_,9),
              [](_,2,8),
              [](1,_,_))).



problem(2, []([]( _,44,41, _, _, _, _),
              []( _,43, _,28,29, _, _),
              []( _, 1, _, _, _,33, _),
              []( _, 2,25, 4,34, _,36),
              [](49,16, _,23, _, _, _),
              []( _,19, _, _,12, 7, _),
              []( _, _, _,14, _, _, _))). 



% Problems from the book:
% Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

% problem 1 (Practice)
problem(3, []([](_, _,20, _, _),
              [](_, _, _,16,18),
              [](22, _,15, _, _),
              [](23, _, 1,14,11),
              [](_,25, _, _,12))).
              


/*
% problem 2 (Practice)
int r=5;
int c=r;
int puzzle[1..r, 1..c] =
  [
   [0, 0, 0, 0,14],
   [0,18,12, 0, 0],
   [0, 0,17, 4, 5],
   [0, 0, 7, 0, 0],
   [9, 8,25, 1, 0],
   ];
*/
problem(4, []([](_, _, _, _,14),
              [](_,18,12, _, _),
              [](_, _,17, 4, 5),
              [](_, _, 7, _, _),
              [](9, 8,25, 1, _))).


/*
% problem 3 (Beginner)
int r=6;
int c = r;
int puzzle[1..r, 1..c] =
  [
   [_, 26,_,_,_,18],
   [_,_,27,_,_,19],
   [31,23,_,_,14,_],
   [_,33,8,_,15,1],
   [_,_,_,5,_,_],
   [35,36,_,10,_,_]
   ];
*/
problem(5, []([]( _,26, _, _, _,18),
              []( _, _,27, _, _,19),
              [](31,23, _, _,14, _),
              []( _,33, 8, _,15, 1),
              []( _, _, _, 5, _, _),
              [](35,36, _,10, _, _))).



/*
% Problem 15 (Intermediate)
int r=8;
int c=r;
int puzzle[1..r, 1..c] = 
  [
   [64, _, _, _, _, _, _, _],
   [ 1,63, _,59,15,57,53, _],
   [ _, 4, _,14, _, _, _, _],
   [ 3, _,11, _,20,19, _,50],
   [ _, _, _, _,22, _,48,40],
   [ 9, _, _,32,23, _, _,41],
   [27, _, _, _,36, _,46, _],
   [28,30, _,35, _, _, _, _]
   ];
*/
problem(6,[]([](64, _, _, _, _, _, _, _),
             []( 1,63, _,59,15,57,53, _),
             []( _, 4, _,14, _, _, _, _),
             []( 3, _,11, _,20,19, _,50),
             []( _, _, _, _,22, _,48,40),
             []( 9, _, _,32,23, _, _,41),
             [](27, _, _, _,36, _,46, _),
             [](28,30, _,35, _, _, _, _))).
            