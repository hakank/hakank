/*

  Rot 13 (Caesar cipher) in AMPL+CP.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param rot;       # the rotate number 
param len;       # string length
param alpha_len; # length of alphabet
param text{1..len}; # the text

var x{1..len} >= 1 <= alpha_len integer;

#
# constraints
#
s.t. c1{i in 1..len}:
     x[i] = if (text[i] + rot) mod alpha_len = 0 then alpha_len else (text[i] + rot) mod alpha_len
;

data;

param rot := 13;
param len := 10;
param alpha_len := 26;

param text := 
 1 1
 2 2
 3 3
 4 13
 5 14
 6 15
 7 16
 8 24
 9 25
10 26
;

# This is solve by presolve.

option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=split_max outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=1";

solve;

display x;

