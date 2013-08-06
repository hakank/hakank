/*

  Car sequencing in AMPL+CP.

  This is based on the OPL3 model car.mod, 
  via the MiniZinc model http://www.hakank.org/minizinc/car.mzn


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param nbCars;
param nbOptions;

set Cars = 1..nbCars;
set Options = 1..nbOptions;

param demand{Cars};
param options{Options, Cars};
param capacity{Options, 1..2};

param nbSlots;
set Slots = 1..nbSlots;

param optionsDemand{i in Options} = sum{j in Cars} demand[j] * options[i,j];


# decision variables
var slot{Slots} >= 1 <= nbCars integer;
var setup{Options, Slots} binary;

minimize z: sum{s in Cars} s*slot[s];

#
# constraints
#
s.t. c1{c in Cars}:
  demand[c] = count{s in Slots} (slot[s] = c)
;

s.t. c2{o in Options, s in 1..nbSlots - capacity[o,2] + 1}:
  sum{j in s..s + capacity[o,2]- 1} setup[o,j] <= capacity[o,1]
;

s.t. c3{o in Options, s in Slots}:
  # setup[o,s] = options[o,slot[s]] # don't work (element -> simulate with exists)
  exists{i in 1..nbCars}
  slot[s] = i and
  setup[o,s] = options[o,i]
;

s.t. c4{o in Options, i in 1..optionsDemand[o]}:
  sum{s in 1..(nbSlots - i * capacity[o,2])} (setup[o,s]) >=
         (optionsDemand[o] - i * capacity[o,1])
;


data car1.dat;
# data car2.dat; # smaller instance

# data;

# # From OPL, car dat (smallest instance)
# param nbCars := 6;
# param nbOptions := 5;
# param demand  = 
#  1 1
#  2 1
#  3 2
#  4 2
#  5 2
#  6 2
# ;
#
# param options: 1 2 3 4 5 6 :=
#   1   1  0  0  0  1  1 
#   2   0  0  1  1  0  1 
#   3   1  0  0  0  1  0 
#   4   1  1  0  1  0  0 
#   5   0  0  1  0  0  0
# ;
#
# param capacity: 1 2 :=
#   1   1 2 
#   2   2 3 
#   3   1 3 
#   4   2 5 
#   5   1 5
# ;


option show_stats 2;

# option solver gecode;
## option gecode_options "icl=def var_branching=afc_min val_branching=min outlev=1 outfreq=1";
# option gecode_options "icl=def var_branching=size_max val_branching=min outlev=1 outfreq=1";
## option gecode_options "icl=def var_branching=afc_max val_branching=min outlev=1 outfreq=1";

option solver ilogcp;
option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

display slot, setup;
