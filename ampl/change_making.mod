/*
 
 From Martello: "Knapsack Problems"

 Page 3:
 """
  ...usually called Change Making Problem, since it recalls the situation
  of a cashier having to asseble a certain change c using the maximum (or
  minimum) number of coins.
  """ 

 Chapter 5 (page 137ff) is devoted to this problem.

 Example from page 140.


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/


set coins; # := {16, 17, 23, 24, 39, 40};
var x{coins} integer >= 0;
param total;# := 100;
param maxx;
param max_num;

#maximize z:
minimize z:
        #x[i];
        sum{i in coins} x[i];
        # sum{i in coins} i*x[i];

s.t. c1: sum{i in coins} i*x[i] = total;

# state the number of coins
#s.t. c2: sum{i in coins} x[i] = 12;

# or require that the must be atleast one of each coin type
# s.t. c3{i in coins}: i*x[i] >= 1;


data;

set coins := 11, 8, 5, 4, 1;
# It's a little boring with the 1 coin
# set coins := 11, 8, 5, 4, 2;
# hmm, a fibonacci coin series

param total := 29;
#set coins := 1 2 3 5  8 13 21  34  55;
#            1 2 4 8 16 32 64 128 256

#option cplex_options "sensitivity";
option solver cplex;
#option solver lpsolve;
solve;

display z;
display x;

for{i in coins: x[i]>0} {
   printf "%d %d-coins = %d\n", x[i], i, i*x[i];
}

printf "Sum: %d\n", sum{i in coins: x[i]> 0} i*x[i]; 
printf "Number of coins: %d\n", sum{i in coins: x[i]> 0} x[i];



/*
#
# Solve for all sums 1..100 
#
let max_num := 0;
let maxx := 0;
for{i in 1..100} {
   let total := i;
   printf "\nTOTAL: %d\n", i;
   solve;
   if z > maxx then {let maxx := z; let max_num := i};
   for{j in coins: x[j]>0} {
      printf "%d * %d = %d\n", x[j], j, j*x[j];
   }

   printf "\n";


   printf "%d: %d (max: %d num:%d)\n", total, z, maxx, max_num;
} 
*/