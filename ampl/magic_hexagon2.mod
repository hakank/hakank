/*

  Magic Hexagon in AMPL+CP.

  Prob023: Magic Hexagon
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
  http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/

  This is a symbolic version of
     http://www.hakank.org/ampl/magic_hexagon.mod

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set letters;
param n;

param num_sums;
param max_sums;
param sums{1..num_sums, 1..max_sums} symbolic default "-";

param num_less;
param less2{1..num_less, 1..2} symbolic;

var x{letters} >= 1 <= 19 integer;

#
# constraints
#
s.t. c1: alldiff{i in letters} x[i];

s.t. c2{i in 1..num_sums}:
    sum{j in 1..max_sums: sums[i,j] != "-"} x[sums[i,j]] = 38
;

s.t. c3{i in 1..num_less}:
    x[less2[i,1]] < x[less2[i,2]]
;

data;

set letters = a b c d e f g h i j k l m n o p q r s;


param num_sums := 15;
param max_sums := 5;

param sums: 1 2 3 4 5 := 
 1  a  b  c  .  .
 2  d  e  f  g  .
 3  h  i  j  k  l
 4  m  n  o  p  .
 5  q  r  s  .  .
 6  a  d  h  .  .
 7  b  e  i  m  .
 8  c  f  j  n  q
 9  g  k  o  r  .
10  l  p  s  .  .
11  c  g  l  .  .
12  b  f  k  p  .
13  a  e  j  o  s
14  d  i  n  r  .
15  h  m  q  .  . 
;

param num_less := 6;
param less2: 1 2 := 
1  a c
2  a h
3  a l
4  a q
5  a s
6  c h
;


option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display x;

for{i in letters} printf "%d, ", x[i];
printf "\n";