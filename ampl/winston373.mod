/*

Sat Dec 29 18:49:05 2007/hakank@bonetmail.com

Page 373 in Winston OR.

It's a sub problem of the transporation problem, but I saw it as
a little puzzle:
Given the row sums and column sum: what should the numbers be?

     x1 + x2 + x3 = 4
     y1 + y2 + y3 = 5
     3    2    4

What is x* and y*?

cplex:
  2 1 1  = 4
  1 1 3  = 5
  3 2 4

bonmin:
  1 1 2  = 4
  2 1 2  = 5
  3 2 4

lpsolve:
  1 1 2  = 4
  2 1 2  = 5
  3 2 4

minos:
  1 1 2  = 4
  2 1 2  = 5
  3 2 4

snopt:
  1 1 2  = 4
  2 1 2  = 5
  3 2 4


loqo, donlp2 (not correct)
  1 1 1  = 4
  1 1 2  = 5
  3 2 4

glpk:
  1 1 2  = 4
  2 1 2  = 5
  3 2 4

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

param nrows integer >= 1;
param ncols integer >= 1;
set rows := 1..nrows;
set cols := 1..ncols;

param row_sums{rows};
param col_sums{cols};

var x{rows, cols} >= 1 integer;

minimize z: 0;

# row sums 
subject to c_row_sum{r in rows}: sum{c in cols} x[r,c] = row_sums[r];

# columns sums
subject to c_col_sum{c in cols}: sum{r in rows} x[r,c] = col_sums[c];
        
/*
# original formulation
var x1 >= 1 integer;
var x2 >= 1 integer;
var x3 >= 1 integer;
var y1 >= 1 integer;
var y2 >= 1 integer;
var y3 >= 1 integer;

minimize z: 0;
s.t. 
        c1: x1 + x2 + x3 = 4;
        c2: y1 + y2 + y3 = 5;
        c3: x1 + y1 = 3;
        c4: x2 + y2 = 2;
        c5: x3 + y3 = 4;
*/


data;

param nrows := 2;
param ncols := 3;

param row_sums := 
        1 4
        2 5
;

param col_sums :=
        1 3
        2 2
        3 4
;


# option solver cplex;
# option solver bonmin;
# option solver lpsolve;
# option solver loqo;
# option solver donlp2;
# option solver snopt;
# option solver cbc;

solve;
# display _obj;
# display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
# display _conname, _con, _con.lb, _con.ub, _con.slack;

for{r in rows} {
  printf "  ";
  for{c in cols} {
    printf "%d ", x[r,c];
  }
  printf " = %d\n", row_sums[r];
}
printf "  ";
for{c in cols} {
   printf "%d ", col_sums[c];
}
printf "\n";


