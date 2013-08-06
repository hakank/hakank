/*
  2007-12-09
  
  Seseman problem

  (Testing some CLP things)

  A B C 
  D   E
  F G H

  A + B + C = 9
  A + D + F = 9
  C + E + H = 9
  F + G + H = 9
  A+B+C+D+E+F+G+H=24 (or some number)
  Or minimize/maximize

  Minimize: total_sum = 20
  Maximize: total_sum = 32

  No min/max -> total_sum = 32 
  No row sum -> row sum = 8 (a..h is 1)


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

var a>=1;
var b>=1;
var c>=1;
var d>=1;
var e>=1;
var f>=1;
var g>=1;
var h>=1;
var total_sum>=1;
param row_sum>=1;


# minimize obj: total_sum;
maximize obj: total_sum;

/* Constraints */
s.t. abc: a+b+c = row_sum;
s.t. adf: a+d+f = row_sum;
s.t. ceh: c+e+h = row_sum;
s.t. fgh: f+g+h = row_sum;
s.t. total: a+b+c+d+e+f+g+h = total_sum;


data;

param row_sum := 19;

option solver cplex;
solve;


display total_sum,row_sum,a,b,c,d,e,f,g,h;
printf "\n";
printf "total_sum: %d\n", total_sum;
printf "row_sum: %d\n", row_sum;
printf "%d %d %d\n", a, b, c; 
printf "%d   %d\n", d, e; 
printf "%d %d %d\n", f, g, h; 

for {x in 20..22} {
   let row_sum := x;
   solve;
   display total_sum,row_sum,a,b,c,d,e,f,g,h;

}

