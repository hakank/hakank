/*

Fri Dec 28 01:09:59 2007/hakank@bonetmail.com

From Lingo modellen KNAPSACK.lg4:

MODEL:

SETS:
   ITEMS: INCLUDE, WEIGHT, RATING;
ENDSETS

DATA:
   ITEMS          WEIGHT RATING =
    ANT_REPEL        1      2
    BEER             3      9
    BLANKET          4      3
    BRATWURST        3      8
    BROWNIES         3     10
    FRISBEE          1      6
    SALAD            5      4
    WATERMELON      10     10;

   KNAPSACK_CAPACITY = 15;
ENDDATA

MAX = @SUM( ITEMS: RATING * INCLUDE);

@SUM( ITEMS: WEIGHT * INCLUDE) <= 
 KNAPSACK_CAPACITY;

@FOR( ITEMS: @BIN( INCLUDE));


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set items;
var includes{items} binary;

param weight{items} >= 0;
param rating{items} >= 0; # 

param knapsack_capacity;

# max = @sum( items: rating * include);
maximize z:
        sum{i in items} rating[i] * includes[i];

#@sum( items: weight * include) <= 
# knapsack_capacity;

subject to capacity:
        sum{i in items} weight[i] * includes[i] <= knapsack_capacity;


data;

param knapsack_capacity := 15;

param:   items:  weight rating :=
    ant_repel        1      2
    beer             3      9
    blanket          4      3
    bratwurst        3      8
    brownies         3     10
    frisbee          1      6
    salad            5      4
    watermelon      10     10;

#option cplex_options "sensitivity"; 
#option solver cplex;
#option solver knitroampl;
option solver lpsolve;
solve;

display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;


display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;


display weight, rating, includes;
