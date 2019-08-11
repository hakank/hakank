/*

  Heterosquare problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 3,
        heterosquare(N, Mat, RowSums, ColSums, Diag1, Diag2),
        maplist(writeln,Mat),
        writeln(rowsums=RowSums),
        writeln(colsums=ColSums),
        writeln(diag1=Diag1),
        writeln(diag2=Diag2),
        nl,nl,
        fail,
        nl.

go.


heterosquare(N, Mat, RowSums, ColSums, Diag1, Diag2) :-
        writeln(n=N),
        NN #= N*N,
        NNN #= N*N*N,
        new_matrix(N,N,1..NN, Mat),
        flatten(Mat,MatVars),
        
        length(RowSums,N),
        RowSums ins 1..NNN,

        length(ColSums,N),  
        ColSums ins 1..NNN,      

        %% diagonals
        Diag1 in 1..NNN,
        Diag2 in 1..NNN,

        %% all entries in the matrix should be different
        all_different(MatVars),

        %% and all sums should be different
        flatten([RowSums,ColSums,Diag1, Diag2],AllSums),
        all_different(AllSums),

        %% calculate rows/col sums
        maplist(sums,Mat,RowSums),
        transpose(Mat,MatT),  
        maplist(sums,MatT,ColSums),

        %% Diagonals
        diagonal1_slice(Mat,Diag1Slice),
        sum(Diag1Slice,#=,Diag1),

        diagonal2_slice(Mat,Diag2Slice),
        sum(Diag2Slice,#=,Diag2),

        flatten([MatVars, RowSums, ColSums], Vars),
        label(Vars).

sums(X,Sums) :-
        sum(X,#=,Sums).

