/*

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in AMPL+CP.

  This is a standard benchmark in theorem proving.

  http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
  """ 
  Someone in Dreadsbury Mansion killed Aunt Agatha. 
  Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
  are the only ones to live there. A killer always hates, and is no 
  richer than his victim. Charles hates noone that Agatha hates. Agatha 
  hates everybody except the butler. The butler hates everyone not richer 
  than Aunt Agatha. The butler hates everyone whom Agatha hates. 
  Noone hates everyone. Who killed Agatha? 
  """

  Originally from 
  F. J. Pelletier: Seventy-five problems for testing automatic theorem provers. Journal of Automated Reasoning, 2: 191–216, 1986.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n := 3;
param agatha := 1;
param butler := 2;
param charles := 3;

# decision variables
var hates{1..n, 1..n} binary;
var richer{1..n, 1..n} binary;

var Killer >= 1 <= n integer;



#
# constraints
#

# Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
# are the only ones to live there. 

# A killer always hates, and is no richer than his victim. 
s.t. c1: 
    exists{k in 1..n}
        Killer = k and
        hates[k, agatha] = 1 and
        richer[k, agatha] = 0
;

# define the concept of richer: no one is richer than him-/herself
s.t. c2{i in 1..n}:
   richer[i,i] = 0
;

# (contd...) if i is richer than j then j is not richer than i
s.t. c3{i in 1..n, j in 1..n: i != j}:
   richer[i,j] = 1 <==> richer[j,i] = 0
;

# Charles hates noone that Agatha hates. 
s.t. c4{i in 1..n}:
   hates[agatha, i] = 1 ==> hates[charles, i] = 0
;

# Agatha hates everybody except the butler. 
s.t. c5:
   hates[agatha, charles] = 1 and
   hates[agatha, agatha] = 1 and
   hates[agatha, butler] = 0
;

# The butler hates everyone not richer than Aunt Agatha. 
s.t. c6{i in 1..n}:
  richer[i, agatha] = 0 ==> hates[butler, i] = 1
;

# The butler hates everyone whom Agatha hates. 
s.t. c7{i in 1..n}:
   hates[agatha, i] = 1 ==> hates[butler, i] = 1
;

# Noone hates everyone. 
s.t. c8{i in 1..n}:
  sum{j in 1..n} (hates[i,j]) <= 2
;

# Who killed Agatha? 

# Testing the validity of the problem.
# This constraint should yield an infeasible problem
# (and it does).
# s.t. c9: Killer != 1;


data;


option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

printf "\nkiller: %d\n", Killer;

printf "hates:\n";
for{i in 1..n} {
    for{j in 1..n} {
      printf "%d, ", hates[i,j];
    }
    printf "\n";
}

printf "\nricher:\n";
for{i in 1..n} {
    for{j in 1..n} {
      printf "%d, ", richer[i,j];
    }
    printf "\n";
}
printf "\n";
