/*

  Recreational mathematics in AMPL+CP.

  From Pascal Van Henrentyck "The OPL Optimization Programming Language", 
  page 32:
  """
  Consider finding an eight digit number that is a square and remains a square
  when 1 is concatenated in front of its decimal notation.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var n >= 10000000 <= 99999999 integer;
var x >= 0 <= 20000 integer;
var y >= 0 <= 20000 integer;


#
# constraints
#
s.t. c1:
  n = x*x and
  100000000 + n = y*y 
;

data;  



option show_stats 2;


option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display n;
display x;
display y;

