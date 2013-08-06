/*

  Hamming distance in AMPL+CP.

  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;

# decision variables
var a{1..n} binary;
var b{1..n} binary;
var diffs >= 1 <= n integer;

#
# constraints
#
s.t. c1: diffs = count{i in 1..n} (a[i] != b[i]);
s.t. c2: diffs = 2;


data;

param n := 6;


option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

display a, b, diffs;
