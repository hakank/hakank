/*

  Bales of hay problem in AMPL+CP.

  From The Math Less Traveled, 
  "The haybaler", http://www.mathlesstraveled.com/?p=582 
  """
  You have five bales of hay.

  For some reason, instead of being weighed individually, they were weighed 
  in all possible combinations of two. The weights of each of these 
  combinations were written down and arranged in numerical order, without 
  keeping track of which weight matched which pair of bales. The weights, 
  in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.

  How much does each bale weigh? Is there a solution? Are there multiple 
  possible solutions? 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param weights{1..10};

var bales{1..n} >= 1 <= 50 integer;

#
# constraints
#
s.t. c1{i in 2..n}: bales[i-1] < bales[i];

s.t. c2{w in 1..10}:
     exists{i in 1..n, j in i+1..n}
        bales[i] + bales[j] = weights[w]
;


data;

param n := 5;
param weights := 
  1 80
  2 82
  3 83
  4 84
  5 85
  6 86
  7 87
  8 88
  9 90
 10 91;


option solver gecode;
option gecode_options 'var_branching=degree_max val_branching=min outlev=1 outfreq=1';

solve;

for{i in 1..n} {
   printf "%2d ", bales[i];

}
printf "\n";