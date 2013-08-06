/*
  Mon Dec 31 08:31:46 2007/hakank@bonetmail.com

  Transshipmentproblem, Winston OR, page 400ff

  According to W it's a balanced transportation problem where one
  use a dummy city.

Supply points: Memphis 150, Denver 200
Demand points: Los Angeles 130, Boston 130
Transshipment points: New York, Chicago.
delivery [*,*]
:             Boston Chicago Denver 'Los Angeles' Memphis 'New York'    :=
Boston            0       0     0           0         0          0
Chicago           0    9999     0           0         0          0
Denver          130       0     0           0         0          0
'Los Angeles'     0       0     0           0         0          0
Memphis           0       0     0           0         0        130
'New York'        0       0     0         130         0       9869
;


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set cities;
param cost{cities, cities} >= 0;
var delivery{cities, cities} integer >= 0;

param supply{cities} >= 0;
param demand{cities} >= 0;

minimize z:
        sum{i in cities, j in cities} cost[i,j]*delivery[i,j];

subject to c_supply{i in cities}:
        sum{j in cities} delivery[i,j] <= supply[i];
#        sum{j in cities} delivery[i,j] <= supply[i];

subject to c_demand{j in cities}:
        sum{i in cities} delivery[i,j] = demand[j];

#subject to no_diag{i in cities}:
#        delivery[i,i] = 0;

data;

# note the dummy variable
set cities := "Memphis" "Denver" "New York" "Chicago" "Los Angeles" "Boston"; #"Dummy";

param cost: "Memphis" "Denver" "New York" "Chicago" "Los Angeles" "Boston" := # "Dummy":=
"Memphis"        0 1000     8    13    25    28 #0 
"Denver"      1000    0    15    12    26    25 #0
"New York"    1000 1000     0     6    16    17 #0
"Chicago"     1000 1000  1000     0    14    16 #0
"Los Angeles" 1000 1000  1000  1000     0  1000 #0
"Boston"      1000 1000  1000  1000  1000     0 #0
#"Dummy"          0    0     0     0     0     0 0
;

param: supply demand :=
        "Memphis"     150      0   # Supply point
        "Denver"      200      0   # SP
        "New York"    9999    9999   # Transshipment point
        "Chicago"     9999    9999   # TP
        "Los Angeles"   0    130   # Demand point
        "Boston"        0    130   # DP
#        "Dummy"       9999  9999 # Dummy 
;

# option cplex_option "sensitivity";
# option solver cplex;
option solver bonmin;
# option solver lpsolve;

solve;

display z;
display supply, demand;
display cost;

printf "Supply points: Memphis 150, Denver 200\n";
printf "Demand points: Los Angeles 130, Boston 130\n";
printf "Transshipment points: New York, Chicago.\n";
display delivery;

