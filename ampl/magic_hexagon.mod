/*

  Magic Hexagon in AMPL+CP.

  Prob023: Magic Hexagon
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
  http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set letters;
param n;

var x{letters} >= 0 <= 19 integer;

#
# constraints
#
s.t. c1: alldiff{i in letters} x[i];

s.t. c2:
  x['a'] + x['b'] + x['c'] =  38 and
  x['d'] + x['e'] + x['f'] + x['g'] =  38 and
  x['h'] + x['i'] + x['j'] + x['k'] + x['l'] =  38 and 
  x['m'] + x['n'] + x['o'] + x['p'] =  38 and 
  x['q'] + x['r'] + x['s'] =  38 and 
  x['a'] + x['d'] + x['h'] =  38 and 
  x['b'] + x['e'] + x['i'] + x['m'] =  38 and 
  x['c'] + x['f'] + x['j'] + x['n'] + x['q'] =  38 and 
  x['g'] + x['k'] + x['o'] + x['r'] =  38 and 
  x['l'] + x['p'] + x['s'] =  38 and 
  x['c'] + x['g'] + x['l'] =  38 and 
  x['b'] + x['f'] + x['k'] + x['p'] =  38 and 
  x['a'] + x['e'] + x['j'] + x['o'] + x['s'] =  38 and 
  x['d'] + x['i'] + x['n'] + x['r'] =  38 and 
  x['h'] + x['m'] + x['q'] =  38 and 

  x['a'] < x['c'] and
  x['a'] < x['h'] and
  x['a'] < x['l'] and
  x['a'] < x['q'] and
  x['a'] < x['s'] and
  x['c'] < x['h']
;

data;

set letters = a b c d e f g h i j k l m n o p q r s;

option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display x;


for{i in letters} printf "%d, ", x[i];
printf "\n";
