/*

  Safe cracking puzzle in AMPL+CP.

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The code of Professor Smart's safe is a sequence of 9 distinct 
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

   C1 <> 1, C2 <> 2, ..., C9 <> 9

  can you find the correct combination?
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n := 9;

var x{1..n} >= 1 <= 9 integer; 


#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];

s.t. c2{i in 1..n}: x[i] != i;

s.t. c3:
  x[4] - x[6] = x[7] and
  x[1] * x[2] * x[3] = x[8] + x[9] and
  x[2] + x[3] + x[6] < x[8] and
  x[9] < x[8]
;

data;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=med outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

printf "x: ";
for{i in 1..n} {
  printf "%2d ", x[i];
}
printf "\n";
