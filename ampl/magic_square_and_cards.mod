/*

  Magic Squares and Cards in AMPL+CP.

  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var x{1..n, 1..n} integer >= 1 <= 13 integer;
var s >= 0 <= 13*4 integer;

maximize obj: s;

#
# constraints
#

# at most 4 cards of each value in a deck
s.t. c1{i in 1..13}: atmost 4  {r in 1..n, c in 1..n} (x[r,c] = i);

# the standard magic square constraints (sans all_different)
s.t. c2{c in 1..n}: sum{r in 1..n} x[r, c] = s;
s.t. c3{r in 1..n}: sum{c in 1..n} x[r, c] = s;
s.t. c4: sum{i in 1..n} x[i, i] = s;
s.t. c5: sum{i in 1..n} x[i, n + 1 - i] = s;


data;  

param n := 3;

option solver gecode;
option gecode_options 'var_branching=size_max val_branching=max outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

display s;

for{i in 1..n} {
   for {j in 1..n} {
     printf "%3d ", x[i,j];
   }
   printf "\n";
}
