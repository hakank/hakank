/*

http://www.math.niu.edu/~rusin/known-math/99/transport

> Problem:
> I have an unknown 4x4 'grid' of integer values.
> What I do have is the column and row sums.

This a classical 4x4 Hitchcock-Koopmans transportation problem. Think of it
this way: you have 4 plants (the rows) and 4 warehouses (the columns). If
x_{ij} is the
amount shipped from plant i to warehouse j, the ith row sum is the amount
shipped out of  plant i while the j th column sum is the amount shipped
into warehouse j. Presumably, you have specified these in advance (assuming
sum(r_i) = sum)c_j)). It is a fact about such systems that if the r_i and
c_j are all integers, then any so-called BASIC solution has integer-values
x_{ij} values. In this case, a basic solution in which 2n-1 variables are
solved for as functions of the remaining n^2-(2n-1) variables, then all the
latter are set to zero (n=4 in your proboem)

Consult any introductory textbook on Operations Research or Linear
Programming
to learn more about such problems. (The classical problem is to determine a
shipping pattern--the x_{ij}--so as to minimize some total cost function of
the form
\sum_i  \sum_j c_{ij} x_{ij} with specified cost matrix c_{ij}.)

>
>
> My plan was to turn this into a simultanious equation problem my lining
> up the rows into  8 equations of 16 unknowns.
>
> This will of course mean that there are mutliple solutions based on the
> 8 (4 row + 4 column) equation values.  I want these solutions!
> e.g
>
>  a  b  c  d   r1
>  e  f  g  h   r2
>  i  j  k  l   r3
>  m  n  o  p   r4
>  -- -- -- --
>  c1 c2 c3 c4
>
> becomes
>
>  1a + 1b + 1c + 1d + 0e +0f +og +... 0p    = r1
>  1a + 0b +0c +0d + 1e + 0f....             = c1
>
> What methods can I use to generate the solutions ?


Ah, this is the well known transportation problem but without
the costs.

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

/* 
# verbose version
var a >= 0 integer; var b >= 0 integer; var c >= 0 integer; var d >= 0 integer;
var e >= 0 integer; var f >= 0 integer; var g >= 0 integer; var h >= 0 integer;
var i >= 0 integer; var j >= 0 integer; var k >= 0 integer; var l >= 0 integer;
var m >= 0 integer; var n >= 0 integer; var o >= 0 integer; var p >= 0 integer;

param col1 := 100; param col2 := 100; param col3 := 100; param col4 := 100;
param row1 := 100; param row2 := 100; param row3 := 100; param row4 := 100;

minimize z: a+b+c+d + e+f+h+h  + i+j+k+l + m+n+o+p ;

c1: a+b+c+d = row1;
c2: e+f+h+h = row2;
c3: i+j+k+l = row3;
c4: m+n+o+p = row4;

c5: a+e+i+m = col1;
c6: b+f+j+n = col2;
c7: c+h+k+o = col3;
c8: d+h+l+p = col4;

# end verbose version

*/

# general solution
set rows;
set cols;
param min_val;
# if the value are integers there's no need to integer constraint
var x{rows, cols} >= min_val; #  integer;
param rr{rows};
param cc{cols};

minimize z: sum{i in rows, j in cols} x[i,j];
#minimize z{i in rows, j in cols}: x[i,j];
# maximize z: x[1,1];

s.t. c_rows{i in rows}: sum{j in cols} x[i,j] = rr[i];
s.t. c_cols{j in cols}: sum{i in rows} x[i,j] = cc[j];

# instead of the constraint in var declaration
#s.t. no_zero{i in rows, j in cols}: x[i,j] >= 10;


#/*
data;

param min_val := 0;
#set rows := 1 2 3 4 5;
#set cols := 1 2 3 4 5;

param: rows: rr := 
        1 101
        2 200
        3 350 
        4 1400
        5 600
;

param: cols: cc :=
        1 1250
        2 200
        3 300
        4 401
        5 500
;
#*/

#option solver snopt;
#option solver bonmin;
option solver cplex;
#option solver lpsolve;
# option solver loqo;

solve;
display z;

display _varname, _var;
#display _conname, _con;

/*
 # för prat versionen
printf "%3d %3d %3d %3d = %3d\n", a,b,c,d, row1;
printf "%3d %3d %3d %3d = %3d\n", e,f,g,h, row2;
printf "%3d %3d %3d %3d = %3d\n", i,j,k,l, row3;
printf "%3d %3d %3d %3d = %3d\n", m,n,o,p, row4;
printf "----------------------\n";
printf "%3d %3d %3d %3d\n", col1, col2, col3, col4;

*/

for{i in rows} {
  for {j in cols} {
    printf "%3d ", x[i,j];
  }
  printf " = %3d\n", sum{j in cols} x[i,j];

}

printf "------------------\n";
for {j in cols} { 
  printf "%3d ", sum{i in rows} x[i,j];
}
printf "\n";

/*
# för GLPK
data;

param min_val := 0;
set rows := 1 2 3 4 5;
set cols := 1 2 3 4 5;

param rr := 
        1 101
        2 200
        3 350 
        4 400
        5 600
;

param cc :=
        1 250
        2 200
        3 300
        4 401
        5 500
;
*/
