/*

From http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
Katta G. Murty: "Optimization Models for Decision Making", sid 340
Example 7.8.1


This is a variant of subset sum.

7.8.1: 
A bank van had several bags of coins, each containing either
16, 17, 23, 24, 39, or 40 coins. While the van was parked on the 
street, thieves stole some bags. A total of 100 coins were lost. 
It is required tond how many bags were stolen. Formulate this 
as a discrete variable easibility model, and also transform this 
into a 0 1model.

cplex;
16  2
17  4
23  0
24  0
39  0
40  0

I.e. 2 bags with 16 coins and 4 bags with 17 coins.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/


set coins; # := {16, 17, 23, 24, 39, 40};
var x{coins} integer >= 0;
param total;# := 100;

maximize z:
        #sum{i in coins} i*x[i];
        sum{i in coins} x[i];

s.t. c1:
        sum{i in coins} i*x[i] = total;

data;

set coins := 16, 17, 23, 24, 39, 40;
param total := 100;

option cplex_options "sensitivity";
option solver cplex;
#option solver lpsolve;
solve;

display z;
display x;

for{i in coins: x[i]>0.1} {
   printf "%d: %d\n", i, x[i];
}


for{i in coins: x[i]>0.1} {
   printf "%d bags with %d coins.\n", x[i],i;
}


display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up, _var.dual, _var.status;
display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up, _con.dual, _con.status;
