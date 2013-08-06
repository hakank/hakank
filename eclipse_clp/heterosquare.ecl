/*

  Heterosquare problem in ECLiPSe.

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

  Compare the MiniZinc model http://www.hakank.org/minizinc/heterosquare.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(propia).


go :-
        N = 3, % size of the matrix

        dim(Mat,[N,N]),
        Mat :: 1..N*N,

        dim(RowSums, [N]),
        RowSums :: 1..N*N*N,

        dim(ColSums, [N]),  
        ColSums :: 1..N*N*N,      

        % diagonals
        Diag1 :: 1..N*N*N,
        Diag2 :: 1..N*N*N,

        % all entries in the matrix should be different
        flatten_array(Mat,MatFlattened),
        ic_global:alldifferent(MatFlattened),

        % and all sums should be different
        flatten_array([](RowSums, ColSums, Diag1,Diag2), AllSums),
        ic:alldifferent(AllSums),

        % calculate rows sums
        (for(I,1,N), param(Mat,RowSums,N) do
             MM is Mat[I,1..N],
             RowSums[I] #= sum(MM)
        ),


        % calculate column sums
        (for(J,1,N), param(Mat,ColSums,N) do
             MM2 is Mat[1..N,J],
             ColSums[J] #= sum(MM2)
        ),

        % diag sums
        (for(I,1,N), 
         fromto(0,In1, Out1, Diag1),
         fromto(0,In2, Out2, Diag2),
         param(Mat,N) do
             MatII is Mat[I,I],
             Out1 #= In1 + MatII,
             MatINI is Mat[I,N-I+1],
             Out2 #= In2 + MatINI
        ),

        term_variables([Mat,RowSums,ColSums], Vars),
        search(Vars,0,occurrence,indomain_max,complete,[]),
        % writeln(mat:Mat),
        ( for(I,1,N), param(Mat,N) do
             ( for(J,1,N), param(Mat,I) do
                   MM is Mat[I,J],
               printf("%3d ", [MM]), write(" ")
             ),
             nl
        ),
        writeln(rowsums:RowSums),
        writeln(colsums:ColSums),
        writeln(diag1:Diag1),
        writeln(diag2:Diag2).
  
