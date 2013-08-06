/*
  Sun Dec 30 17:18:16 2007/hakank@bonetmail.com

Winston OR, page 414 and 417, shortest path as a transshipment problem.

Here we should deliver from plant 1 to plant 6.

Cf transshipment.mod .

Winston describes two solutions:
  1 -> 2 -> 5 -> 6
  1 -> 3 -> 5 -> 6

Cplex:

path: 1->2 cost: 4
path: 2->5 cost: 2
path: 3->3 cost: 0
path: 4->4 cost: 0
path: 5->6 cost: 2

(skip i->i):
  1->2->5->6
with a cost of 5.

If using <= supply we get the other solution
  1->3->5->6


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


set plants;
param cost{plants, plants} >= 0;
var delivery{plants, plants} integer >= 0;

param supply{plants} >= 0;
param demand{plants} >= 0;

param temp;
param temp_count;

minimize z:
        sum{i in plants, j in plants} cost[i,j]*delivery[i,j];

subject to c_supply{i in plants}:
        sum{j in plants} delivery[i,j] = supply[i];

subject to c_demand{j in plants}:
        sum{i in plants} delivery[i,j] = demand[j];



#subject to no_diag{i in plants}:
#        delivery[i,i] = 0;

data;

# note the dummy variable
set plants := 1 2 3 4 5 6;

param cost: 1 2 3 4 5 6 :=
        1   0     4   3 999 999 999
        2   999   0 999   3   2 999 
        3   999 999   0 999   3 999
        4   999 999 999   0 999   2
        5   999 999 999 999   0   2
        6   999 999 999 999 999   0

;

# 1 is supply point, 6 is demand point, the rest is
# transshipment points, which has dummy supply/demand.
param: supply demand :=
        1   1  0
        2   1  1
        3   1  1
        4   1  1
        5   1  1
        6   0  1
;

# option cplex_option "sensitivity";
option solver cplex;
# option solver bonmin;
#option solver lpsolve;


solve;

display z;
display supply, demand;
display cost;

display delivery;

for{i in plants, j in plants: delivery[i,j] > 0.2} {
   printf "path: %d->%d cost: %d\n", i,j, cost[i,j];
}



let temp := 1;  # start node
let temp_count := 0;
repeat {
        printf "(%d) ", temp;
        for{j in plants: j <> temp and delivery[temp,j] > 0} {
           printf "%2d -> %2d\n",temp,j;
           let temp := j;
           break;
        }
        let temp_count := temp_count + 1;
        if temp = 1 or temp_count > card(plants)  then break;
        if temp >= card(plants) then break; # last node
}

printf "It was %d nodes in the cycle.\n", temp_count;

