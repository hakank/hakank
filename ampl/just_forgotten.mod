/*

  Just forgotten puzzle (Enigma 1517) in AMPL+CP.

  From http://www.f1compiler.com/samples/Enigma%201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
  
  Joe was furious when he forgot one of his bank account numbers. 
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:

      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7 
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5

  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it?
  """
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param rows;
param cols;
param a{1..rows, 1..cols};

var x{1..cols} >= 0 <= 9  integer;

#
# constraints
#
s.t. c1: alldiff{c in 1..rows} x[c];

s.t. c2{r in 1..rows}: exactly 4 {c in 1..cols} (x[c] = a[r,c]);
# This variant give the same statistics.
# s.t. c2b{r in 1..rows}: count {c in 1..cols} (x[c] = a[r,c]) = 4;

data;

param rows := 4;
param cols := 10;

param a: 1 2 3 4 5 6 7 8 9 10 := 
  1  9 4 6 2 1 5 7 8 3 0 
  2  8 6 0 4 3 9 1 2 5 7 
  3  1 6 4 0 2 9 7 8 5 3 
  4  6 8 2 4 3 1 9 0 7 5
;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=max outlev=1 outfreq=1 timelimit=60';
# option solver ilogcp;

solve;

for{c in 1..cols} {
  printf "%2d ", x[c];
}
printf "\n";
