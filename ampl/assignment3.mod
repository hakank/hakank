/*
Sun Dec 30 07:56:15 2007/hakank@bonetmail.com

Winston OR, Assignment Problems

Problem 3, page 399

"""
Tom Cruise, Freddy Prinze Jr, Harrison Ford, and Matt LeBlanc
are marooned on a desert island with Jennifer Anniston,
Courtney Cos, Gwynneth Paltrow, and Julia Roberts.
The 'compatibility matrix' in Table 52 indicate how much happiness
each couple would experience if the spend all their time toghether.
The happiness earned by a couple is proportional to the fraction 
of time the spend toghether. 
...
The optimal solution requires that that each person send all their
time with one person of the opposite sex, so this result is often
referred to as the Marriage Theorem.
"""

cplex:
z = 30

0 0 1 0
0 1 0 0
0 0 0 1
1 0 0 0
     Tom Cruise: Gwynneth Paltrow
Freddie Prinz Jr: Courtney Cox
  Harrison Ford: Julia Roberts
   Mark LeBlanc: Jennifer Anniston


donlp2 give an intresting solution
z = 30

0.50 0.00 0.50 0.00
0.00 1.00 0.00 0.00
0.00 0.00 0.00 1.00
0.50 0.00 0.50 0.00
     Tom Cruise: Jennifer Anniston
     Tom Cruise: Gwynneth Paltrow
Freddie Prinz Jr: Courtney Cox
  Harrison Ford: Julia Roberts
   Mark LeBlanc: Jennifer Anniston
   Mark LeBlanc: Gwynneth Paltrow


as does loqo:
z = 30

0.51 0.00 0.49 0.00
0.00 1.00 0.00 0.00
0.00 0.00 0.00 1.00
0.49 0.00 0.51 0.00
     Tom Cruise: Jennifer Anniston
     Tom Cruise: Gwynneth Paltrow
Freddie Prinz Jr: Courtney Cox
  Harrison Ford: Julia Roberts
   Mark LeBlanc: Jennifer Anniston
   Mark LeBlanc: Gwynneth Paltrow


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

# the general version
param rows >= 0;
param cols >= 0;
var x{1..rows, 1..cols} binary;
param cost{1..rows, 1..cols};
param male{1..rows} symbolic;
param female{1..cols} symbolic;

# minimize the cost
maximize z:
        sum{i in 1..rows, j in 1..cols} x[i,j]*cost[i,j];

subject to c_rows{i in 1..rows}:
        sum{j in 1..cols} x[i,j] = 1;

subject to c_cols{j in 1..cols}:
        sum{i in 1..rows} x[i,j] = 1;

data;

param rows:= 4; # rows: swimmers
param cols:= 4; # columns: times to minimize

param male :=
        1 "Tom Cruise"
        2 "Freddie Prinz Jr"
        3 "Harrison Ford"
        4 "Mark LeBlanc"
;

param female := 
        1 "Jennifer Anniston"
        2 "Courtney Cox"
        3 "Gwynneth Paltrow"
        4 "Julia Roberts"
;

param cost: 1 2 3 4 := 
      1 7 5 8 2
      2 7 8 8 4
      3 3 5 7 9
      4 5 5 6 7
;

# option presolve 0;
# option cplex_option "sensitivity";
option solver cplex;
# option solver bonmin;
#option solver donlp2;
# option solver snopt;
# option solver loqo;

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

for{i in 1..rows, j in 1..cols: x[i,j] > 0.01} {
   printf "% 15s: %s\n", male[i], female[j];

}
