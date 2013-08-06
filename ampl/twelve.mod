/*
Example: Two draughts at each corner and one draught at each edge.
Xpress-Mosel Model

model 'twelve'

! Description  : Twelve draughts puzzle
! Source       : Boris Kordemsky - The Moscow Puzzles (P36)
! Date written : MAGIC 16/12/92, Xpress-MP 22/6/98, Mosel 16/4/03
! Written by   : M J Chlond 

  uses 'mmxprs'

 Twelve draught pieces are arranged in a square frame with four on each side. Try
 placing them so there are 5 on each side. (Kordemsky)

Maybe this problem is not described very well but I wanted to stick with the orig
inal text from Kordemsky. The problem may be stated in terms of guards on the wal
l of a square fort. If a guard stands on a side wall then he may only watch that
particular wall whereas a guard at a corner may watch two walls. If twelve guards
 are positioned such that there are two on each side wall and one at each corner
then there are four guards watching each wall. How can they be rearranged such th
at there are five watching each wall?
)

Xpress:
x =
0 2 0 3 
2 0 0 0 
0 0 0 0 
3 0 0 2 

cplex:
x =
0 0 0 5
2 0 0 0
0 0 0 0
3 0 2 0

lpsolve
x =
0 2 0 3
0 0 0 2
0 0 0 0
5 0 0 0


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


param s := 4;

set S := 1..s;
var x{S,S} integer >= 0;

  # arbitrary objective
minimize any: x[2,2];
  
  # total of 12 pieces placed
s.t.  cona: sum{i in S,j in S} x[i,j] = 12;
  
  # 5 pieces on each side 
s.t.  conb: sum{j in S} x[1,j] = 5;
s.t.  conc: sum{i in S} x[i,1] = 5;
s.t.  cond: sum{j in S} x[4,j] = 5;
s.t.  cone: sum{i in S} x[i,4] = 5;
  
  # inner squares unused 
s.t.  conf: sum{i in 2..3,j in 2..3} x[i,j] = 0;

#option solver cplex;
option solver lpsolve;

solve;
  
  # display results
  printf "x =\n";
  for{i in S} {
    for{j in S} {
      printf "%d ", x[i,j];
    }
    printf "\n";
  }
