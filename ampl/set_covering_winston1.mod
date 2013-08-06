/*
  Tue Jan  1 22:13:05 2008/hakank@bonetmail.com

  Winston OR, set covering page 486.

  Place as few firestations as possible to cover all cities
  within the range of 15.

  The book enumerates the nearby stations, but it's more
  general to let the model figure that out.

  Solution:
    2 fire stations: in city 2 and 4.

  If the minimum distance minimiavståndet is 20, 
  lpsolve yields city 2 and 3. 

  For the distance 25 it's enough with 1 station in city 6 
  (not surprising since the max distance to all citites are
  25).

  For distance 10 it requires 4 stations: 2,3,4,5.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


set cities;
var x{cities} binary; 
param distance{cities, cities} >= 0;
param min_distance >= 0;

minimize z: 
        sum{i in cities} x[i];

subject to c1{j in cities}:
        sum{i in cities: distance[i,j] <= min_distance} x[i] >= 1; 

data;

param min_distance := 15;
set cities := 1 2 3 4 5 6;
param distance: 1 2 3 4 5 6 :=
        1 0 10 20 30 30 20
        2 10 0 25 35 20 10
        3 20 25 0 15 30 20
        4 30 35 15 0 15 25
        5 30 20 30 15 0 14
        6 20 10 20 25 14 0
;




option cplex_options "sensitivity writeprob=xxx.lp";
option solver cplex;
# option solver minos;
# option solver cbc;
#  option solver snopt;
#option solver gjh;
#option solver ipopt;
# option solver bonmin;
# option solver lpsolve;
# option solver donlp2;
# option solver loqo;

solve;

#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;

display z;
display x;

for{i in cities: x[i] > 0.1}{
    printf "City %d should have a fire station.\n", i;
}
