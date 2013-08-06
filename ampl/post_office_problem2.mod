/*
   http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

   Post office problem

   This finds the optimal solution for minimizing the number of full-time
   employees to the post office problem

   See post_office_problem.mod.

   Version 2 is a more general model:
  - only one row with 5 day constraint (using mod)
  - consider that it's more expensive with work sat/sun
      Cost is
        0 17 100 Mon
        1 13 100 Tue
        2 15 100 Wed
        3 19 100 Thu
        4 14 100 Fri 
        5 16 150 Sat
        6 11 200 Sun
    Note: the cost is the cost to start a certain day, not
    the cost to work a specific day.

    This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
    See also my AMPL page: http://www.hakank.org/ampl/

*/

/* sets */
set DAYS;

/* parameters */
param Need {DAYS};
param dayName{DAYS} symbolic;
param Cost{DAYS};

/* Decision variables. x[i]: No. of workers starting at day i */
var x {i in DAYS} >= 0 integer;

/* objective function */
# minimize z1: sum{i in DAYS} x[i]*Cost[i];

minimize z2:
   sum{i in DAYS, j in DAYS: j <> (i+5) mod 7 and j <> (i+6) mod 7} x[i] * Cost[j]; 


subject to fivedays{i in DAYS}:
        sum{j in DAYS: j <> (i+5) mod 7 and j <> (i+6) mod 7 } x[j] >= Need[i]; 
        ;
        

subject to helg1{i in 0..1}:
        x[i+5] <= x[i+3];

data;

# set DAYS:= Mon Tue Wed Thu Fri Sat Sun;

#param Need:=
#Mon             17
#Tue             13
#Wed             15
#Thu             19
#Fri             14
#Sat             16
#Sun             11;

# Experimenting with cost:
# Cost per day is 100 (normal salary = 5*100=500)
# Working Saturdays costs 100 extra, and Sunday 200 extra.
param: DAYS: Need Cost dayName:=
        0 17 500 Mon
        1 13 600 Tue
        2 15 800 Wed
        3 19 800 Thu
        4 14 800 Fri 
        5 16 800 Sat
        6 11 700 Sun
;


# option solver lpsolve;
option solver cplex;
solve;

printf "Monday is 0:\n";

display _obj, x;
display  dayName, x.lb, x, x.ub, x.rc;
printf "Number of persons: %d\n", sum{i in DAYS} x[i];

printf "Number of persons working each day:\n";
for{i in DAYS} {
        printf "%s Requirement: %d Works: %d\n", dayName[i], Need[i], sum{j in DAYS: j <> (i+5) mod 7 and j <> (i+6) mod 7} x[j]; 
}
