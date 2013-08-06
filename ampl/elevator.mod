/*

Sat Dec 29 01:20:54 2007/hakank@bonetmail.com
http://ite.pubs.informs.org/Vol6No3/Chlond/
A Tokyo Elevator Puzzle
 
 
Martin J.Chlond

When I ran it in OPL:

Post-processing final solution:
1 1 1 0 0 0 
1 0 0 1 1 0 
0 1 0 1 0 1 
1 0 1 0 0 1 
0 1 0 0 1 1 
0 0 1 1 1 0 
Done post-processing

Feasible solution with objective = 10:
x = [1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0];

Feasible solution with objective = 6:
x = [0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0];

Final solution with objective = 6:
x = [0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 1 1 0 0 0];]]]
       2       6     9       13   16 17     

dvs 2 6 9 13 16 17

Using this model:
cplex
x = [0 0 1 0 0 0 1 0 1 0 1 0 0 0 0 1 1 0 0 0]
3 7 9 11 16 17
1 1 1 0 0 0
1 0 0 1 1 0
0 1 0 1 0 1
0 0 1 1 0 1
1 0 0 0 1 1
0 1 1 0 1 0

It's not the same solution...

Bonmin:
x = [0 1 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 1]
2 4 6 12 18 20
1 1 1 0 0 0
1 1 0 1 0 0
0 0 1 1 1 0
1 0 0 0 1 1
0 0 1 1 0 1
0 1 0 0 1 1

lpsolve:
x = [1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0 0 ]
1 2 10 16 17 18
1 1 1 0 0 0
1 1 0 1 0 0
1 0 0 0 1 1
0 1 0 0 1 1
0 0 1 1 1 0
0 0 1 1 0 1

GLPSOL give the same solution as bonmin:
x = [0 1 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 1 ]
2 4 6 12 18 20
1 1 1 0 0 0
1 1 0 1 0 0
0 0 1 1 1 0
1 0 0 0 1 1
0 0 1 1 0 1
0 1 0 0 1 1
]


LaGO:
x = [0 0 1 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 ]
3 4 5 11 19 20
1 1 1 0 0 0
1 1 0 1 0 0
0 0 1 1 1 0
0 0 1 1 0 1
1 0 0 0 1 1
0 1 0 0 1 1
]

Cbc:
x = [1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0 0 ]
1 2 10 16 17 18
1 1 1 0 0 0
1 1 0 1 0 0
1 0 0 0 1 1
0 1 0 0 1 1
0 0 1 1 1 0
0 0 1 1 0 1
]


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/



*/

/* 
 Model name   : elevator.mod
 Description  : solves Elevator puzzles
 Source       : The Tokyo Puzzles - Kobon Fujimura 
 Date written : 7/8/06
 Written by   : Martin Chlond, Lancashire Business School
 Email        : mchlond@uclan.ac.uk 
*/

param m integer >= 0;
param n integer >= 0;
 
#param M{1..m};
#param N{1..n};

param c{1..m,1..n};
var x{1..m} binary;

minimize z:
        sum{i in 1..m} x[i];

# each pair of floors connected by at least one elevator */
subject to c1{j in 1..n,k in 1..n}:
        sum{i in 1..m} c[i,j]*c[i,k]*x[i] >= 1;



data;

#
# Data set for elevator puzzle
# 6 floors, 3 visits (excluding top and bottom)
# 

param m := 20;
param n := 6;

# notera hur man definierar denna matrix!
# (jag förstår dock inte riktigt vad den representerar)
param c:   1 2 3 4 5 6:= 
        1  1 1 1 0 0 0 
        2  1 1 0 1 0 0 
        3  1 1 0 0 1 0 
        4  1 1 0 0 0 1 
        5  1 0 1 1 0 0 
        6  1 0 1 0 1 0 
        7  1 0 1 0 0 1 
        8  1 0 0 1 1 0 
        9  1 0 0 1 0 1 
        10 1 0 0 0 1 1 
        11 0 1 1 1 0 0 
        12 0 1 1 0 1 0 
        13 0 1 1 0 0 1 
        14 0 1 0 1 1 0 
        15 0 1 0 1 0 1 
        16 0 1 0 0 1 1 
        17 0 0 1 1 1 0 
        18 0 0 1 1 0 1 
        19 0 0 1 0 1 1
        20 0 0 0 1 1 1 
;



#option cplex_option "sensitivity";
# option solver cplex;
# option solver bonmin;
option solver lpsolve;
# option solver LaGO;
# option solver cbc;

solve;

display x;

#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;

printf "x = [";
for{i in 1..m} printf "%d ", x[i];
printf "]\n";

for{i in 1..m:x[i] = 1} printf "%d ", i;
printf "\n";

for{j in 1..n} {
    for{i in 1..m: x[i] = 1} {
        # printf "%d %d %d\n", i,j, c[i,j];
        printf "%d ", c[i,j];
    }
    printf "\n";
}

/*
  for(j in N) {
    for(i in M) {
      if(x[i]==1) {
       write(c[i][j]);
       write(" ");
      }
    }
    writeln();

*/
