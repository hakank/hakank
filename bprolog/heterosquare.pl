/*

  Heterosquare problem in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 3,
        heterosquare(N, Mat, RowSums, ColSums, Diag1, Diag2),
        foreach(I in 1..N, J in 1..N,[MM],
            (MM is Mat[I,J],
             format("~3d ", [MM]),
             (J =:= N -> nl ;  true)
            )),
        writeln(rowsums:RowSums),
        writeln(colsums:ColSums),
        writeln(diag1:Diag1),
        writeln(diag2:Diag2),
        nl,nl,
        fail.


heterosquare(N, Mat, RowSums, ColSums, Diag1, Diag2) :-

        new_array(Mat,[N,N]),
        array_to_list(Mat,MatVar),
        MatVar :: 1..N*N,

        length(RowSums, N),
        RowSums :: 1..N*N*N,

        length(ColSums, N),  
        ColSums :: 1..N*N*N,      

        % diagonals
        Diag1 :: 1..N*N*N,
        Diag2 :: 1..N*N*N,

        % all entries in the matrix should be different
        alldifferent(MatVar),

        % and all sums should be different
        term_variables([RowSums, ColSums, Diag1,Diag2], AllSums),
        alldifferent(AllSums),

        % calculate rows sums
        foreach(I in 1..N,
                RowSums[I] #= sum([Mat[I,J] : J in 1..N])
        ),

        % calculate column sums
        foreach(J in 1..N,
                ColSums[J] #= sum([Mat[I,J] : I in 1..N])
        ),

        Diagonal1 @= Mat^diagonal1,
        Diag1 #= sum(Diagonal1),
        Diagonal2 @= Mat^diagonal2,
        Diag2 #= sum(Diagonal2),


        term_variables([MatVar,RowSums,ColSums], Vars),
        labeling(Vars).
  