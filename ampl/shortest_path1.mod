/*
  Sun Dec 30 17:18:16 2007/hakank@bonetmail.com

  Winston OR, sid 415. Shortest path.

The problem is when to buy/sell a car depending of the costs
for maintenance and selling the old car.

We do this as a transshipment-problem (Winston page 417 and
transshipment*.mod): creating transshipment points between
1 and 6 which has 1 as supply and demand.

Cplex:
path:
1 3 12
2 2 0
3 5 12
4 4 0
5 6 7

i.e. 1 -> 3 -> 5 -> 6 with a cost of 31

With a transhipmentsupply/demand on 11 instead of 1:

1 3 12
2 2 0
3 3 0
3 4 7
4 4 0
4 6 12
5 5 0

i.e. 1->3->4->6 with a cost of på 31

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param time >= 0;
param new_car >= 0;
param trade_in{1..time} >= 0;
param maintenance {0..time-1} >= 0;
param supply{1..time+1} >= 0;
param demand{1..time+1} >= 0;

param temp;
param temp_count;

# Cost matrix
var cost{1..time, 1..time+1} integer >= 0;# default new_car^2;

# "delivery table" (in transshipment talk)
var x{1..time, 1..time+1} >= 0 integer;

# minimize the cost
minimize z:
        sum{i in 1..time, j in 1..time+1} x[i,j]*cost[i,j];

# cost matrix
# cost[i,j] =   maintenance cost incurred during years i, i+1,..j-1 
#             + cost of purchasing new car at beginning of year i 
#             - trade-in value received at beginning of year j
subject to cost_matrix{i in 1..time, j in i+1..time+1}:
  cost[i,j] = new_car + (sum{k in 0..j-i-1} maintenance[k]) - trade_in[j-i]
;

# yields new_car^2 in lower diagonal
subject to cost_matrix2{i in 1..time, j in 1..time+1: j < i}:
       cost[i,j] = new_car^2;
;

# rows represents time
subject to c_rows{i in 1..time}:
        sum{j in 1..time+1} x[i,j] = supply[i];
        ;

# what strategy
subject to c_cols{j in 1..time+1}:
        sum{i in 1..time} x[i,j]  = demand[j];
;

data;

param time := 5;
param new_car := 12;

# car trade-in price, in 1000 dollar
param trade_in :=
1  7
2  6
3  2
4  1
5  0
;

# cost to repair the car year x (in 1000 dollar)
param maintenance :=
  0  2
  1  4
  2  5
  3  9
  4 12
;

# "transshipment-table" which is the key to this type
# of graph problem (with this approach)
# 2..5 are transshipments points with unlimited supply and demand
param: supply demand :=
        1 1 0
        2 11 11
        3 11 11
        4 11 11
        5 11 11
        6 0 1
;
 

#option cplex_option "sensitivity";
option solver cplex;
#option solver bonmin;
# option solver lpsolve;
# option solver cbc;
# option solver snopt;


option display_transpose 0;

solve;

display z;
#display cost;
# display x;

printf "\ncost:\n";
for{i in 1..time} {
 printf "%d: ", i;
 for{j in 1..time+1} {
   # printf "%d ", ceil(x[i,j]);
   printf "%3d ", cost[i,j];
 }
 printf "\n";
  
}

printf "\nx:\n";
for{i in 1..time} {
 printf "%d: ", i;
 for{j in 1..time+1} {
   # printf "%d ", ceil(x[i,j]);
   printf "%d ", x[i,j];
 }
 printf "\n";

}


printf "\npath (ignore i->i):\n";

for{i in 1..time, j in 1..time+1: x[i,j] > 0.1} {
  printf "%d %d %d\n", i,j, cost[i,j]; 
}


# a little more intelligent loop 
let temp := 1;  # the start node
let temp_count := 0;
repeat {
        printf "(%d) ", temp;
        for{j in 1..time+1: j <> temp and x[temp,j] > 0} {
           printf "%2d -> %2d\n",temp,j;
           let temp := j;
           break;
        }
        let temp_count := temp_count + 1;
        if temp = 1 or temp_count > time  then break;
        if temp > time then break; # last node
}

printf "It was %d nodes in the cycle.\n", temp_count;

