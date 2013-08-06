/*

  Talisman Square in SICStus Prolog.

  http://mathworld.wolfram.com/TalismanSquare.html
  """
  An nXn array of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or 
  diagonally, without wrapping around) is greater than or equal to
  some value k is called a n,k)-talisman square. 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/talisman_square.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/talisman_square.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 5,
        K = 4,
        talisman_square(N, K, X),
        pretty_print(X),
        fail.

talisman_square(N,K,X) :-

        N2 is N*N,
        matrix(X,[N,N]),
        append(X,XList),
        domain(XList,1,N2), 

        all_different(XList),

        ( for(I,2,N),
          param(X,K,N) do 
              ( for(J,2,N), 
                param(X,K,I) do
                    matrix_element(X,I,J,XIJ),
                    I1 is I-1,
                    matrix_element(X,I1,J,XI1J),
                    J1 is J-1,
                    matrix_element(X,I,J1,XIJ1),
                    abs(XIJ-XI1J) #>= K,
                    abs(XIJ-XIJ1) #>= K
              )
        ),

        ( for(I,1,N-1),
          param(X,K,N) do 
              ( for(J,1,N-1), 
                param(X,K,I) do
                    matrix_element(X,I,J,XIJ),
                    I1 is I+1,
                    matrix_element(X,I1,J,XI1J),
                    J1 is J+1,
                    matrix_element(X,I,J1,XIJ1),
                    abs(XIJ-XI1J) #>= K,
                    abs(XIJ-XIJ1) #>= K
              )
        ),


        % some symmetry breaking
        matrix_element(X,1,1,1),

        labeling([], XList).



matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


pretty_print(X) :-
        ( foreach(Row,X) do
              ( foreach(R, Row) do
                    format('~d\t',[R])              
              ),
              nl
        ),
        nl.

% pretty_print(X) :-
%         dim(X, [N,N]),
%         ( for(I, 1, N), param(X, N) do
%             ( for(J, 1, N), param(X, I) do
%                 XX is X[I,J],
%                 printf("%2d", XX),
%                 write(" ")
%             ),
%             nl
%         ),nl.        
