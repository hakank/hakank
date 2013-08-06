/*

From http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
Katta G. Murty: "Optimization Models for Decision Making", sid 340
Example 7.8.2


7.8.2: 
A merchant has bags of emeralds (nine to a bag), and rubies
(four to a bag). He has a total of 59 jewels. Required to find how 
many of his jewels are rubies. Formulate as a 0 1 integer 
feasibility system.)


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/


set coins := {4,9};
var x{coins} integer >= 0;

maximize z:
        sum{i in coins} i*x[i];
        #sum{i in coins} x[i];
        # sum{i in coins} x[i];

s.t. c1:
        sum{i in coins} i*x[i] = 59;


#option solver cplex;
option solver lpsolve;
solve;

display x;

for{i in coins: x[i]>0.1} {
   printf "%d: %d\n", i, x[i];
}
printf "Of which are rubies: %d\n", 4*x[4];

                         

