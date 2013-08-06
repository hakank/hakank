/*
   Magic Square variant from Taha Operations Research, 
   page 367 (solution page 778)

   It's the alldifferent variant I'm interested in here.

   An either-or constraint transforms from
     either 
          x[i] >= x[j] + p[j] 
     or 
          x[j] >= x[i] + p[i]

   to the two constraints

     M *     y[i,j]  + (x[i] - x[j]) >= p[j] 
     M * (1 -y[i,j]) + (x[j] - x[i]) >= p[i] 
 
   Tahas solution is
     2 9 4 
     7 5 3
     6 1 8


cplex:
8 3 4
1 5 9
6 7 2


lpsolve:
8 1 6
3 5 7
4 9 2


glpk:
8 3 4
1 5 9
6 7 2

glpk med --intopt:
8 1 6
3 5 7
4 9 2


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

param n := 3;
set range := 1..n;
var x{range, range} integer >= 1 <= n^2;
var y{range,range} binary; # for either or
# var y{range,range, range,range} binary; # for either or

# var total integer >= 0;
param total := 15;
param M := 100;

# dummy objective
maximize z: 0;

# rows
c1{i in range}: sum{j in range} x[i,j] = total;
# columns
c2{j in range}: sum{i in range} x[i,j] = total;
# diagonal 1
c3: sum{i in range} x[i,i] = total;
# diagonal 2
# c4: x[3,1] + x[2,2] + x[1,3] = total;
# general diagonal 2
c4: sum{i in range} x[i, card(range)-i+1] = total;

# all_different (distinct)
# no solver can handle the or constraints
# all_different{i in range, j in range}: 
#   (x[i,j] >= x[j,i] + 1) or (x[i,j] <= x[j,i] - 1);

# transform to two big M constraints: M * y and M*(1-y)
all_different1{i in range, j in range}: 
   M*y[i,j]       + (x[i,j] - x[j,i]) >= (if i <> j then 1);

all_different2{i in range, j in range}: 
   M*(1 - y[i,j]) + (x[j,i] - x[i,j]) >= (if i <> j then 1);

# tests another variant (with more variables)
#all_different1{i in range, j in range,  k in range, l in range}: 
#   M*y[i,j, k,l]       + (x[i,j] - x[k,l]) >= (if i <> k then 1);

#all_different2{i in range, j in range,  k in range, l in range}: 
#   M*(1 - y[i,j, k,l]) + (x[k,l] - x[i,j]) >= (if i <> k then 1);



option solver cplex;
#option solver lpsolve;
# option solver bonmin;
solve;

display x;

printf "Taha's solution (for n=3):\n";
printf "2 9 4\n";
printf "7 5 3\n";
printf "6 1 8\n";
printf "\nThis solution:\n";

for{i in range} {
  for{j in range} {
      printf "%d ", x[i,j];
  }  
  printf "\n";
}

printf "Total: %d\n", total;

