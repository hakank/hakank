/*

  Map coloring problem in AMPL+CP.

  See http://www.hakank.org/ampl/map2.mod for an alternative encoding.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

set countries;
param n;
param connections{countries, countries} binary default 0;

var x{countries} >= 1 <= n integer;


#
# constraints
#
s.t. c1{i in countries, j in countries: connections[i,j] = 1}: 
     x[i] != x[j];

# Symmetry breaking
s.t. c2: x["belgium"] = 1;
s.t. c3: x["denmark"] <= 2;


data;

set countries = belgium denmark france germany netherlands luxembourg;
param n := 6;
param connections: belgium denmark france germany netherlands luxembourg :=
 belgium     . . 1 1 1 1
 denmark     . . . 1 . .
 france      1 . . 1 1 .
 germany     1 1 1 . 1 1
 netherlands 1 . 1 1 . .
 luxembourg  1 . . 1 . .
;


option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "x:\n";
for{i in countries} {
   printf "%2d ", x[i];
}

printf "\n";
