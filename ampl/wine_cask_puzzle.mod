/*
   Chlond's Wine cask puzzle

Answer:
         Full      3/4      1/2       1/4      Empty

Son 1      3         1        1         1         3
Son 2      2         2        1         2         2
Son 3      2         1        3         1         2
Son 4      1         3        1         3         1
Son 5      1         2        3         2         1

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


param nephew   = 5;
param casktype = 5;

set N = 1..nephew;
set C = 1.. casktype;
param howfull{C};
param cdum{C};
var x{N,C} integer >= 1;
var dtot{N};

# definieras i datasegmentet
#param howfull := 0 .25 .5 .75 1; 
#param cdum := 10000 1000 100 10 1;

#  any:= x(1,1)

subject to nocon{i in N}:
    sum{j in C} x[i,j] = 9;

subject to amcon{i in N}:
    sum{j in C} howfull[j]*x[i,j] = 4.5;

subject to nccon{j in C}:
    sum{i in N} x[i,j] = 9;

subject to cdummy{i in N}:
    sum{j in C} cdum[j]*x[i,j] = dtot[i];

subject to order{i in 2..nephew}:
    dtot[i-1] - dtot[i] >= 1;


maximize any:
   x[1,1];
#  sum{i in N, j in C} x[i,j];

data;

param: howfull cdum := 
      1 0    10000
      2 0.25  1000
      3 0.5    100
      4 0.75    10
      5 1        1;

#param cdum := 
#      1 10000 
#      2 1000 
#      3 100 
#      4 10 
#      5 1;


options solver cplex;
# options solver cbc;
solve;

display x, dtot;


print "Correct answer should be:";
print "";
print "        Full      3/4      1/2       1/4      Empty";
print "";
print "Son 1      3         1        1         1         3";
print "Son 2      2         2        1         2         2";
print "Son 3      2         1        3         1         2";
print "Son 4      1         3        1         3         1";
print "Son 5      1         2        3         2         1";

for {i in N} {
  for{j in C} {
     printf "%d ", x[i,j];
  }
  printf "\n";

}

for {i in N} {
     printf "%d ", dtot[i];
}
printf "\n";

