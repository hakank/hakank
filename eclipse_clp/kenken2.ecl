/*

  KenKen in ECLiPSe.

  http://en.wikipedia.org/wiki/KenKen
  """
  KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing 
  several characteristics with sudoku. The name comes from Japanese and 
  is translated as "square wisdom" or "cleverness squared".
  ...
  The objective is to fill the grid in with the digits 1 through 6 such that:

    * Each row contains exactly one of each digit
    * Each column contains exactly one of each digit
    * Each bold-outlined group of cells is a cage containing digits which 
      achieve the specified result using the specified mathematical operation: 
        addition (+), 
        subtraction (-), 
        multiplication (x), 
        and division (Ã·). 
        (Unlike in Killer sudoku, digits may repeat within a group.)

  ...
  More complex KenKen problems are formed using the principles described 
  above but omitting the symbols +, -, x and Ã·, thus leaving them as 
  yet another unknown to be determined.
  """


  Compare with the following models:
  * Comet: http://www.hakank.org/comet/kenken.co
  * Comet: http://www.hakank.org/comet/kenken2.co (more general version).


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(listut).
:-lib(ic_search).
:-lib(propia).


/*

  Note: This model is an experiment in using Propia, i.e. 
  the two infers in calc/3.

  Without Propia it is very slow, which is probably an indication
  that is a wrong approach...

*/

go :-
        problem(1,N, Problem),
        kenken(N, Problem).
        

kenken(N, Problem) :-

        length(Problem,Len),

        % decision variables
        dim(X,[N,N]),
        X :: 1..N,

        % all rows and columns must be unique
        ( for(I,1,N), param(X,N) do
              ic_global:alldifferent(X[I, 1..N]),
              ic_global:alldifferent(X[1..N, I])
        ),
        

        ( for(I,1,Len), param(Problem,X) do
              % extract the hints
              nth1(I,Problem,Hint),
              [Result, Coeffs] = Hint,

              % calculate this hint
              calc(Result, Coeffs,X)
        ),

        term_variables(X, Vars),
        search(Vars, 0, first_fail, indomain, complete, [backtrack(Backtracks)]),
        pretty_print(X),
        writeln(backtracks:Backtracks).



calc(Result, Coeffs,X) :-
        length(Coeffs, Len),
        (
            % special handling for size 2
            Len == 2,
            [[AR,AC],[BR,BC]] = Coeffs,
            A is X[AR,AC],
            B is X[BR,BC],
            check2(A,B,Result) infers most
        )
        ;
        (
            % or size > 2
            (foreach([R,C],Coeffs),
             foreach(S, CoeffRes),
             param(X) do
                 XRC is X[R,C],
                 S = XRC
            ),
            % and use Propia here as well
            check_many(Result, CoeffRes) infers most
        ).


%
% all alternatives for 2 argument 
% I assume that only segments with 2 cells can be 
% minus or div.
% 
check2(A,B,Result) :- A + B #= Result.  
check2(A,B,Result) :- A * B #= Result.
check2(A,B,Result) :- A * Result #= B. % B/A = Result
check2(A,B,Result) :- B * Result #= A. % A/B = Result
check2(A,B,Result) :- A - B #= Result.
check2(A,B,Result) :- B - A #= Result.

% either sum or product
check_many(Result, CoeffRes) :- Result #= sum(CoeffRes).
check_many(Result, CoeffRes) :- Result #= prod(CoeffRes).

% product of a list
prod(List, Product) :-
        (foreach(L, List),
         fromto(1,In,Out,Prod) do
             Out = In * L
        ),
        eval(Prod) #= Product.


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
% State the problem, i.e. the hints. 
%
% For a better view of the problem, see 
%  http://en.wikipedia.org/wiki/File:KenKenProblem.svg  
%
%
%   The solution is:
%     5 6 3 4 1 2
%     6 1 4 5 2 3
%     4 5 2 3 6 1
%     3 4 1 2 5 6
%     2 3 6 1 4 5
%     1 2 5 6 3 4
%
problem(1, 6, [
               [ 11, [[1,1], [2,1]]],
               [  2, [[1,2], [1,3]]],
               [ 20, [[1,4], [2,4]]],
               [  6, [[1,5], [1,6], [2,6], [3,6]]],
               [  3, [[2,2], [2,3]]],
               [  3, [[2,5], [3,5]]],
               [240, [[3,1], [3,2], [4,1], [4,2]]],
               [  6, [[3,3], [3,4]]],  
               [  6, [[4,3], [5,3]]],
               [  7, [[4,4], [5,4], [5,5]]],
               [ 30, [[4,5], [4,6]]],  
               [  6, [[5,1], [5,2]]],
               [  9, [[5,6], [6,6]]],
               [  8, [[6,1], [6,2], [6,3]]],
               [  2, [[6,4], [6,5]]]
           ]).
           

