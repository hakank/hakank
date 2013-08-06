/*
Sun Dec 30 07:56:15 2007/hakank@bonetmail.com

Winston OR, Assignment Problems, page 393f

The example shows 4 machines and 4 jobs, but
I generalized it so there are 'rows' rows
and 'cols' columns. All rows must be assigned
so cols >= rows, but not all columns has to 
be assigned.

Later I added a column for testing.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

/*
# Talkative version
var x11 >= 0; # should be binary but is not needed
var x12 >= 0; 
var x13 >= 0; 
var x14 >= 0; 

var x21 >= 0; 
var x22 >= 0; 
var x23 >= 0; 
var x24 >= 0; 

var x31 >= 0; 
var x32 >= 0; 
var x33 >= 0; 
var x34 >= 0; 

var x41 >= 0; 
var x42 >= 0; 
var x43 >= 0; 
var x44 >= 0; 


minimize z:
        14*x11 +  5*x12 + 8*x13 +  7*x14 +
         2*x21 + 12*x22 + 6*x23 +  5*x24 +
         7*x31 +  8*x32 + 3*x33 +  9*x34 + 
         2*x41 +  4*x42 + 6*x43 + 10*x44;

subject to
        c1: x11 + x12 + x13 + x14 = 1;   # machine constraints
        c2: x21 + x22 + x23 + x24 = 1;
        c3: x31 + x32 + x33 + x34 = 1;
        c4: x41 + x42 + x43 + x44 = 1;

        c5: x11 + x21 + x31 + x41 = 1;   # job constraints
        c6: x12 + x22 + x32 + x42 = 1;
        c7: x13 + x23 + x33 + x43 = 1;
        c8: x14 + x24 + x34 + x44 = 1;
*/

# the general version
param rows >= 0;
param cols >= 0;
var x{1..rows, 1..cols} binary;
param cost{1..rows, 1..cols};

# minimize the cost
minimize z:
        sum{i in 1..rows, j in 1..cols} x[i,j]*cost[i,j];

#
# if using 'maximize' instead, the cost will mean
# profit, grades, etc.
#
# maximize z:
#        sum{i in 1..rows, j in 1..cols} x[i,j]*cost[i,j];


#
# exact one per row: all rows must be assigned
# 
subject to c_rows{i in 1..rows}:
        sum{j in 1..cols} x[i,j] = 1;

# Change to atmost one per column.
# I.e. not all columns must be assigned.
subject to c_cols{j in 1..cols}:
        sum{i in 1..rows} x[i,j] <= 1;

data;

param rows:= 4; # rows
param cols:= 5; # columns

# added the 5th column
param cost: 1 2 3 4 5 := 
      1  14  5 8  7 15
      2   2 12 6  5  3
      3   7  8 3  9  7
      4   2  4 6 10  1
;




option presolve 0;
#option cplex_option "sensitivity";
option solver cplex;
# option solver bonmin;
solve;
display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;

/*
printf "%d %d %d %d\n", x11,x12,x13,x14;
printf "%d %d %d %d\n", x21,x22,x23,x24;
printf "%d %d %d %d\n", x31,x32,x33,x34;
printf "%d %d %d %d\n", x41,x42,x43,x44;
*/

for{i in 1..rows} {
 for{j in 1..cols} {
   # printf "%d ", ceil(x[i,j]); # om man använder minos etc
   printf "%d ", x[i,j];
 }
 printf "\n";
  
}