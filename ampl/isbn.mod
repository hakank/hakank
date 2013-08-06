/*

  Some explorations of ISBN13 in AMPL+CP.

  See http://en.wikipedia.org/wiki/ISBN

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

param p{1..n-1}; # the ISBN to check

# decision variables
var isbn{1..n} >= 0 <= 9 integer;
var mult0 >= 1 <= 9 integer;
var mult1 >= 1 <= 9 integer;


#
# constraints
#
s.t. c0{i in 1..n-1}: isbn[i] = p[i];

s.t. c1:
     isbn[n] = (10 - (sum{i in 1..n-1} 
                       if i mod 2 = 0 then
                         isbn[i] * mult0 # 3
                        else
                          isbn[i] * mult1 # 1
                     ) mod 10) mod 10
;

# 
# mult0 = 3 and mult1 = 1. Can we find it?
# s.t. c2: mult0 = 3;
# s.t. c3: mult1 = 1;
s.t. c4: isbn[n] = 0;

data;

param n := 13;

param p := 
   1 9
   2 7 
   3 8
   4 1 
   5 5
   6 5
   7 8
   8 6 
   9 0
  10 8
  11 9
  12 0
  # 13 0
; 

# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";


solve;

display isbn, mult0, mult1;


