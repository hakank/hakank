/*
http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

Post office problem

From "Operations Research":
"""
    A post office requires a different number of full-time employees working on different days of the week [summarized below]. Union rules state that each full-time employee must work for 5 consecutive days and then receive two days off. For example, an employee who works on Monday to Friday must be off on Saturday and Sunday. The post office wants to meet its daily requirements using only full-time employees. Minimize the number of employees that must be hired.
"""
To summarize the important information about the problem:

    * Every full-time worker works for 5 consecutive days and takes 2 days off
    * Day 1 (Monday): 17 workers needed
    * Day 2 : 13 workers needed
    * Day 3 : 15 workers needed
    * Day 4 : 19 workers needed
    * Day 5 : 14 workers needed
    * Day 6 : 16 workers needed
    * Day 7 (Sunday) : 11 workers needed

The post office needs to minimize the number of employees it needs to hire to meet its demand. 

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

/* sets */
set DAYS ordered; # ordered don't work in glpk

/* parameters */
param Need {i in DAYS};

/* Decision variables. x[i]: No. of workers starting at day i */
var x {i in DAYS} >= 0 integer;

/* objective function */
minimize z: sum{i in DAYS} x[i];

/* Constraints */
s.t. mon: sum{i in DAYS: i<>'Tue' and i<>'Wed'} x[i] >= Need['Mon'];
s.t. tue: sum{i in DAYS: i<>'Wed' and i<>'Thu'} x[i] >= Need['Tue'];
s.t. wed: sum{i in DAYS: i<>'Thu' and i<>'Fri'} x[i] >= Need['Wed'];
s.t. thu: sum{i in DAYS: i<>'Fri' and i<>'Sat'} x[i] >= Need['Thu'];
s.t. fri: sum{i in DAYS: i<>'Sat' and i<>'Sun'} x[i] >= Need['Fri'];
s.t. sat: sum{i in DAYS: i<>'Sun' and i<>'Mon'} x[i] >= Need['Sat'];
s.t. sun: sum{i in DAYS: i<>'Mon' and i<>'Tue'} x[i] >= Need['Sun'];


data;

set DAYS:= Mon Tue Wed Thu Fri Sat Sun;

param Need:=
Mon             17
Tue             13
Wed             15
Thu             19
Fri             14
Sat             16
Sun             11;

option solver cplex;
solve;

display  x,z;

for{i in DAYS} printf "%s: %d\n", i, x[i];
printf "\n";
printf "z: %d\n", sum{i in DAYS} x[i];
