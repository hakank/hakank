/*
Sat Jan  5 20:23:21 2008/hakank@bonetmail.com

Xpress-Mosel Model

model 'abbott'

! Description  : The Abbott's Window
! Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.  
! Date written : 29/11/99
! Written by   : M J Chlond 

The site says that this is a solution:

0  0  0  0  0  0  0  0
0  1  0  0  0  0  1  0
0  0  1  0  0  1  0  0
0  0  0  1  1  0  0  0
0  0  0  1  1  0  0  0
0  0  1  0  0  1  0  0
0  1  0  0  0  0  1  0
0  0  0  0  0  0  0  0


When I run XPress this is the solution:
1 1 1 1 1 1 1 1 
1 1 1 1 1 1 1 1 
0 1 1 1 1 1 1 0 
1 1 1 1 1 1 1 1 
0 1 1 0 1 0 1 0 
1 0 1 0 1 1 1 1 
1 0 1 1 0 1 1 1 
1 1 1 1 0 0 1 1 

With AMPL and cplex;
1   1   1   1   1   1   1   1
1   0   1   1   1   1   0   1
1   1   0   1   1   1   0   1
1   1   1   0   1   1   1   0
1   1   1   0   1   1   1   0
1   1   0   1   1   1   0   1
1   0   1   1   1   1   0   1
1   1   1   1   1   1   1   1

I think that this is a solutions as well.

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param row := 8;
param col := 8;
  
set R := 1..row;
set C := 1..col;
var x{R,C} binary;  # x(i,j) = 1 if window {i,j} open, else 0
var a{R} integer <= 4 >= 0;
var b{C} integer <= 4 >= 0;
var c{1..row-2} integer <= 4 >= 0;  
var d{1..col-1} integer <= 4 >= 0;
var e{1..col-1} integer <= 4 >= 0;
var f{1..row-2} integer <= 4 >= 0;

#s.t. open: 
#        sum{i in R,j in C} x[i,j];

s.t. rcon{i in R}:
        sum{j in C} x[i,j] = 2*a[i];
    
s.t. ccon{j in C}:
        sum{i in R} x[i,j] = 2*b[j];

s.t. c4{i in 2..row-1}:
    sum{k in 1..i} x[k,i-k+1] = 2*c[i-1];
    
s.t. ddcon{j in 1..col-1}:
        sum{k in j..row} x[k,col-k+j] = 2*d[j];

s.t. decon{j in 1..col-1}:
        sum{k in 1..row-j+1} x[k,j+k-1] = 2*e[j];

s.t. dfcon{i in 2..row-1}:
        sum{k in i..row} x[k,k-i+1] = 2*f[i-1];

s.t. ca: x[1,1] = 1;
s.t. cb: x[row,1] = 1;
s.t. cc: x[1,col] = 1;
s.t. cd: x[row,col] = 1;


maximize z:
        sum{i in R,j in C} x[i,j]
        # open
;


/*  
  forall(i in R) do
    forall(j in C)
      write(getsol(x[i,j]),' ')
    writeln
  end-do
*/



# data;

#option presolve 0;
# to write problem to a .lp-fil set writeprob=xxx.lp
# option cplex_options "sensitivity";
option solver cplex;
#option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver LaGO;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display x,a,b,c,d,e,f;
# expand;
display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
