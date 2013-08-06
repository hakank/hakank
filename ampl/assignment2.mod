/*
Sun Dec 30 07:56:15 2007/hakank@bonetmail.com

Winston OR, Assignment Problems

Problem 2 sid 398

"""
Doc Councillman is putting together a relay team for
the 400-meter relay. Each swimmer must swin 100 meter
of breaststroke, backstroke, butterfly or freestyle.
Doc believes that each swimmer will attain the times
given in table 51. To minimize the team's time for the race,
which swimmer should swim which stroke.
"""

(Refers to Machol 1970 "An application  of the Assignment Problem")

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

# the general version
param rows >= 0;
param cols >= 0;
var x{1..rows, 1..cols} binary;
param cost{1..rows, 1..cols};
param name{1..rows} symbolic;
param type{1..cols} symbolic;

# minimize the cost
minimize z:
        sum{i in 1..rows, j in 1..cols} x[i,j]*cost[i,j];

subject to c_rows{i in 1..rows}:
        sum{j in 1..cols} x[i,j] = 1;

subject to c_cols{j in 1..cols}:
        sum{i in 1..rows} x[i,j] = 1;

data;

param rows:= 4; # rows (swimmer)
param cols:= 4; # columns (time to minimize)

param name :=
        1 "Gary Hall"
        2 "Mark Spitz"
        3 "Jim Montgomery"
        4 "Chet Jastremski"
;

param type := 
        1 "free"
        2 "breast"
        3 "fly"
        4 "back"
;

param cost: 1 2 3 4 := 
      1 54 54 51 53
      2 51 57 52 52
      3 50 53 54 56
      4 56 54 55 53
;

# option presolve 0;
#option cplex_option "sensitivity";
option solver cplex;
# option solver bonmin;
solve;
#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;

display z;
for{i in 1..rows} {
 for{j in 1..cols} {
   # printf "%d ", ceil(x[i,j]); # om man använder minos etc
   printf "%d ", x[i,j];
 }
 printf "\n";
  
}


for{i in 1..rows, j in 1..cols: x[i,j] > 0} {
   printf "% 15s: %s\n", name[i], type[j];

}
