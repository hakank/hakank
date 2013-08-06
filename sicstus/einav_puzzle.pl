/*

  A programming puzzle from Einav in SICStus Prolog.

  
  From 
  "A programming puzzle from Einav"
  http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
  """
  My friend Einav gave me this programming puzzle to work on. Given this array of positive and negative numbers:
  33   30  -10 -6  18   7  -11 -23   6
  ...
  -25   4  16  30  33 -23  -4   4 -23
 
  You can flip the sign of entire rows and columns, as many of them
  as you like. The goal is to make all the rows and columns sum to positive
  numbers (or zero), and then to find the solution (there are more than one)
  that has the smallest overall sum. So for example, for this array:
  33  30 -10
  -16  19   9
  -17 -12 -14
  You could flip the sign for the bottom row to get this array:
  33  30 -10
  -16  19   9
  17  12  14
  Now all the rows and columns have positive sums, and the overall total is 
  108.
  But you could instead flip the second and third columns, and the second 
  row, to get this array:
  33  -30  10
  16   19    9
  -17   12   14
  All the rows and columns still total positive, and the overall sum is just 
  66. So this solution is better (I don't know if it's the best)
  A pure brute force solution would have to try over 30 billion solutions. 
  I wrote code to solve this in J. I'll post that separately.
  """

  Compare with the following model:
  * http://www.hakank.org/minizinc/einav_puzzle.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :- 
        problem(Data),
        matrix(Data,[Rows,Cols]),
        
        matrix(X,[Rows,Cols]),
        append(X,XList),
        domain(XList,-100,100),
        
        % row/column sums
        length(RowSums,Rows),
        length(ColSums,Cols),
        domain(RowSums,0,300),
        domain(ColSums,0,300),

        % the signs of rows and column
        length(RowSigns,Rows),
        length(ColSigns,Cols),
        ( foreach(R,RowSigns) do R in {-1,1}),
        ( foreach(C,ColSigns) do C in {-1,1}),

       
        % total sum (to minimize)
        TotalSum in 0..1000,
        ( foreach(XRow,X),
          foreach(DataRow1,Data),
          foreach(RowSign1,RowSigns),
          fromto(0,In0,Out0,TotalSum),
          param(ColSigns) do
              ( foreach(XR,XRow),
                foreach(ColSign1,ColSigns),
                foreach(D1,DataRow1),
                fromto(0,In1,Out1,ThisSum1),
                param(RowSign1) do
                    XR #= D1*RowSign1*ColSign1,
                    Out1 #= In1 + XR           
              ),
              Out0 #= In0 + ThisSum1
        ),

        % row sums
        ( foreach(RowSum2, RowSums),
          foreach(RowSign2,RowSigns),
          foreach(DataRow2,Data),
          param(ColSigns) do
              ( foreach(ColSign2,ColSigns),
                foreach(D2,DataRow2),
                fromto(0,In2,Out2,ThisSum2),
                param(RowSign2) do
                    Out2 #= In2 + D2*ColSign2*RowSign2
              ),
              RowSum2 #= ThisSum2
        ),

         % col sums
         transpose(Data,DataTransposed),
        ( foreach(ColSign3,ColSigns),
          foreach(ColSum3,ColSums),
          foreach(DataColumn3,DataTransposed),
          param(RowSigns) do
              ( foreach(RowSign3,RowSigns),
                foreach(D3,DataColumn3),
                fromto(0,In3,Out3,ThisSum3),
                param(ColSign3) do
                    Out3 #= In3 + D3*ColSign3*RowSign3
              ),
              ColSum3 #= ThisSum3
        ),

        append(XList,RowSigns,Vars1),
        append(Vars1,ColSigns,Vars2),
        append(Vars2,RowSums,Vars3),
        append(Vars3,ColSums,Vars),
        labeling([ffc,bisect,up,minimize(TotalSum)],Vars),

        ( foreach(Row,X) 
        do 
          write(Row),nl
        ),
        write(total_sums:TotalSum),nl,
        write(row_sums:RowSums),nl,
        write(col_sums:ColSums),nl,
        write(row_signs:RowSigns),nl,
        write(col_signs:ColSigns),nl,nl,
        % fail,
        fd_statistics.

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


problem(
[[33,30,10,-6,18,-7,-11,23,-6],
 [16,-19,9,-26,-8,-19,-8,-21,-14],
 [17,12,-14,31,-30,13,-13,19,16],
 [-6,-11,1,17,-12,-4,-7,14,-21],
 [18,-31,34,-22,17,-19,20,24,6],
 [33,-18,17,-15,31,-5,3,27,-3],
 [-18,-20,-18,31,6,4,-2,-12,24],
 [27,14,4,-29,-3,5,-29,8,-12],
 [-15,-7,-23,23,-9,-8,6,8,-12],
 [33,-23,-19,-4,-8,-7,11,-12,31],
 [-20,19,-15,-30,11,32,7,14,-5],
 [-23,18,-32,-2,-31,-7,8,24,16],
 [32,-4,-10,-14,-6,-1,0,23,23],
 [25,0,-23,22,12,28,-27,15,4],
 [-30,-13,-16,-3,-3,-32,-3,27,-31],
 [22,1,26,4,-2,-13,26,17,14],
 [-9,-18,3,-20,-27,-32,-11,27,13],
 [-17,33,-7,19,-32,13,-31,-2,-24],
 [-31,27,-31,-29,15,2,29,-15,33],
 [-18,-23,15,28,0,30,-4,12,-32],
 [-3,34,27,-25,-18,26,1,34,26],
 [-21,-31,-10,-13,-30,-17,-12,-26,31],
 [23,-31,-19,21,-17,-10,2,-23,23],
 [-3,6,0,-3,-32,0,-10,-25,14],
 [-19,9,14,-27,20,15,-5,-27,18],
 [11,-6,24,7,-17,26,20,-31,-25],
 [-25,4,-16,30,33,23,-4,-4,23]]).

