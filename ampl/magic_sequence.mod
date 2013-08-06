/*

  Magic sequence in AMPL+CP.

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

var s{0..n-1} integer >= 0 <= n-1 integer;

#
# constraints
#
s.t. c1{i in 0..n-1}: exactly s[i] {j in 0..n-1} (s[j] = i);
s.t. c2: n = sum{i in 0..n-1} s[i];
s.t. c3: n = sum{i in 0..n-1} s[i]*i;

data;  

# for{k in 10..20} {
#
#     let n := k;
#     printf "\nn: %d\n", n;
#
#     option solver gecode;
#     option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';
#
#     solve;
#
#     # display s;
#     for{i in 0..n-1} { printf "%2d ", s[i]; }
#     printf "\n\n";
# }

param n := 1000;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

for{i in 0..n-1} { printf "%2d ", s[i]; }
printf "\n\n";
