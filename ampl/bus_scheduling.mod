/*
   Bus scheduling, Taha OR, page 58

   Place the number of buses during certain periods with different  
   requirements on the number of buses in the traffic.

   Taha's solution:
     26 buses
     x1 = 4, 
     x2 = 10
     x4 = 8
     x5 = 4

Cplex:
x1 = 4
x2 = 10
x3 = 0
x4 = 8
x5 = 4
x6 = 0

OK.

General solution
z = 26
x [*] :=
1   4
2  10
3   0
4   8
5   4
6   0

OK


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

/*
# talkative formulation
var x1 integer >= 0; # number of buses starting at 12:01 am
var x2 integer >= 0; #                              4:01 am
var x3 integer >= 0; # 8:01
var x4 integer >= 0; # 12:01 pm
var x5 integer >= 0; # 4:01 pm
var x6 integer >= 0; # 8:01 pm

minimize z: x1 + x2 + x3 + x4 + x5 + x6;

# Time periods
c1: x1 +                  x6 >= 4; # 12:01 .. 4:00 am (around the clock)
c2: x1 + x2 >= 8;
c3:      x2 + x3 >= 10;
c4:           x3 + x4 >= 7; 
c5:                x4 + x5 >= 12;
c6:                     x5 + x6 >= 4;
*/

# general approach
param time_slots;
param demands{1..time_slots};
var x{1..time_slots} integer >= 0;

minimize z: sum{i in 1..time_slots} x[i];

# constraints
c1{i in 1..time_slots-1}: x[i]+x[i+1] >= demands[i];

# around the clock
c2: x[time_slots] + x[1] = demands[time_slots]; 

data;

param time_slots := 6;
param demands :=
        1  8
        2 10
        3  7
        4 12
        5  4
        6  4 # x6 + x1
;


option solver cplex;
solve;

# display z,x1,x2,x3,x4,x5,x6;
display z;
display x;


