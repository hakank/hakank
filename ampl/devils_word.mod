/*

  Devil's word in AMPL+CP.

  I.e. addition/subtraction of an array of numbers to give a specific total (e.g. 666)

  Compare to my CGI program "Devil's Word"
  http://www.hakank.org/data_snooping/666.cgi

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param arr{1..n};

# decision variables
var plus{1..n} binary;
var minus{1..n} binary;
var total >= 0 integer;
var result{i in 1..n} >= -max{1..n} arr[i] <= min{1..n} arr[i] integer;

# number of minus entries (perhaps to be minimized)
var num_minus >= 0 integer;

minimize obj: num_minus;

#
# constraints
#

# calculate the sum of the numbers in arr
s.t. c1:
  total = sum{i in 1..n} (arr[i]*plus[i] + (-arr[i])*minus[i])
  # total = sum{i in 1..n} (result[i]) # alternative summation
;

# either plus and minus
s.t. c2{i in 1..n}: plus[i] + minus[i] = 1;

# calculate the result array
s.t. c3{i in 1..n}:  result[i] = arr[i]*plus[i] + (-arr[i])*minus[i];

s.t. c4:  num_minus = sum{i in 1..n} minus[i];

s.t. c5: total = 666;

data;


# My name ("Håkan Kjellerstrand") in ASCII numbers.
# Cf http://www.hakank.org/data_snooping/666.cgi?name=H%E5kan+Kjellerstrand&submit=ok
# which gives the solution:
# +72+229+107+97+110+32+75-106+101+108-108+101-114-115-116-114+97+110+100 = 666
#
# There are 288 different solutions...
#
param n := 19;
param total := 666;
param arr := 
1    72 # H
2   229 # å
3   107 # k
4    97 # a
5   110 # n
6    32 # <space>
7    75 # K
8   106 # j
9   101 # e
10  108 # l
11  108 # l
12  101 # e
13  114 # r
14  115 # s
15  116 # t
16  114 # r
17   97 # a
18  110 # n
19  100 # d
;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display plus, minus, total, result, num_minus;
