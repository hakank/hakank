/*

  Map coloring problem in AMPL+CP.

  See http://www.hakank.org/ampl/map.mod for an alternative encoding.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

set countries;
param n; # number of colors
set connections within {countries cross countries};
var x{countries} >= 1 <= n integer;

#
# constraints
#
s.t. c1{ (i,j) in connections}:
        x[i] != x[j];

# Symmetry breaking
s.t. c2: x["Belgium"] = 1;
s.t. c3: x["Denmark"] <= 2;


data;

param n := 4; # number of colors

set countries = Belgium Denmark France Germany Netherlands Luxembourg Sweden Norway Finland Spain Portugal Poland Switzerland Austria Italy;

set connections := 
        France Belgium
        France Luxembourg
        France Germany
        France Spain
        Spain Portugal
        Luxembourg Germany
        Luxembourg Belgium
        Belgium Netherlands
        Belgium Germany
        Germany Netherlands
        Germany Denmark
        Germany Poland
        Germany Austria
        Denmark Sweden
        Sweden Norway
        Sweden Finland
        Sweden Poland
        Finland Norway
        France Switzerland
        France Italy
        Italy Switzerland
        Italy Austria
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
