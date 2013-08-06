/*

 Set covering, generated data from set_cov1.dat

 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/

*/

set persons;
set group;
param belongs{group, persons} default 0;

var x{persons} binary;

minimize z: sum{i in persons} x[i];
# maximize z: sum{i in persons} x[i];

#s.t. c1{i in group}: sum{j in persons} x[j]*belongs[i,j] <= 1;
# s.t. c2{i in group}: sum{j in persons} x[j]*belongs[i,j] = 1;
s.t. c3{i in group}: sum{j in persons} x[j]*belongs[i,j] >= 1;

data set_cov1.dat;


option show_stats 2;
option solver cplex;
# option solver bonmin; # slow
#option solver lpsolve;
solve;

#display belongs;
#display x;

for{j in persons: x[j] = 1} {
    printf "Person %d: ", j;
    for{i in group: belongs[i,j] = 1} {
      printf "\t%d ", i;
    }
   printf "\n";
}

#display z;

printf "The following groups are not represented (should be none):\n";
for{i in group} {

    if sum{j in persons} x[j]*belongs[i,j] = 0 then 
        printf "%d ",  i;
}
printf "\n";


 printf "Number of persons per group:\n";
for{i in group} {
    printf "%d(%d) ", i, sum{j in persons} x[j]*belongs[i,j];
}
printf "\n";

end;
