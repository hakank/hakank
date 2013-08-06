/*

  Pairs divides the sum puzzle in AMPL+CP.

  
  From comp.lang.prolog
  """
  Date: Sat, Feb 28 2009 3:55Â am
  From: Nick Wedd

  Here is a puzzle which I found surprisingly easy to program Prolog to
  generate solutions to.  If any of you teach Prolog to students, you
  might use it as an example (like the goat-wolf-cabbage thing).

  Find a set of four distinct positive integers such that, for every pair
  of them, their difference divides their sum.

  Find lots of such sets.

  As above, but sets of five distinct positive integers.
  
  As above, but sets of six ...
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param max_val;

# decision variables
var x{1..n} >= 1 <= max_val integer;
var z >= 1 <= max_val*n integer;

# minimize obj: z;

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];
s.t. c2{i in 2..n}: x[i-1] < x[i]; # ordered
s.t. c3: z = sum{i in 1..n} x[i];
s.t. c4: z mod n = 0;
s.t. c5{i in 1..n,j in 1..n: i < j}:
  z mod abs(x[i]-x[j]) = 0
  # and  abs(x[i]-x[j]) > 1 # extra: not allowing differences of 1.
;

data;  

param n := 7;
param max_val := 100;

option show_stats 2;
# option presolve 0;


option solver gecode;
option gecode_options 'var_branching=degree_size_min val_branching=min outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display x;
display z;


