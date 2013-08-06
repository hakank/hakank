/*

  Heterosquare problem in SICStus Prolog.

  From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
  """
  A heterosquare of order n is a n*n square whose elements are
  distinct integers from 1 to n^2 such that the sums of the rows,
  columns and diagonals are all different. Here is an example of
  heterosquare of order 3 
             19
  
  1  2  3    6
  8  9  4    21
  7  6  5    18
  
  16 17 12   15  (Sums)
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/heterosquare.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/heterosquare.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 8, % size of the matrix

        matrix(Mat,[N,N]),
        append(Mat,MatList),
        N2 is N*N,
        N3 is N*N*N,
        domain(MatList,1,N2),

        length(RowSums, N),
        domain(RowSums,1,N3),

        length(ColSums, N),  
        domain(ColSums,1,N3),

        % diagonals
        Diag1 in 1..N3,
        Diag2 in 1..N3,
        
        % all entries in the matrix should be different
        all_different(MatList),
        
        % and all sums should be different
        append(RowSums, ColSums, AllSums1),
        append(AllSums1,[Diag1,Diag2], AllSums),
        all_different(AllSums),
        
        % rows sums
        ( foreach(Row,RowSums),
          foreach(M,Mat) do
              sum(M,#=,Row)
        ),

        % column sums
        transpose(Mat,MatTransposed),
        ( foreach(Col,ColSums),
          foreach(M,MatTransposed) do
              sum(M,#=,Col)
        ),
                
        % diag sums
        ( for(I,1,N), 
          fromto(0,In1, Out1, Diag1),
          fromto(0,In2, Out2, Diag2),
          param(Mat,N) do
              matrix_element(Mat,I,I,MatII),
              Out1 #= In1 + MatII,
              NI1 is N-I+1,
              matrix_element(Mat,I,NI1,MatIN1),
              Out2 #= In2 + MatIN1
        ),

        % search
        append(MatList,RowSums,Vars1),
        append(Vars1,ColSums, Vars2),
        append(Vars2,[Diag1,Diag2], Vars),
        % maximum(Max,Vars),
        % labeling([ffc,bisect,up,minimize(Max)],Vars),
        labeling([ffc,bisect,up],Vars),

        % output
        ( foreach(M,Mat) do
              write(M),nl
        ),
        write(rowsums:RowSums),nl,
        write(colsums:ColSums),nl,
        write(diag1:Diag1),nl,
        write(diag2:Diag2),nl,nl,
        fd_statistics.


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



