/*
   From Applications of Optimization with XPress-MP.

   Page 58.
"""
A burglar sees eight items, of different values and weights. He wants to take 
the items of greatest total value whose total weight is not more than the 
maximum WTMAX he can carry. 
We introduce binary variables takei for all i in the set of all items (ITEMS) 
to represent the decision whether item i is taken or not. takei has the value 
1 if item i is taken and 0 otherwise. 
"""


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

# param NumItems integer >= 0;
set ITEMS;                  # Index range for items

param WTMAX integer, >= 1;  # Maximum weight allowed
param VALUE{ITEMS} >= 0;    # Value of items
param WEIGHT{ITEMS} >= 0;   # Weight of items

param accum_weight default 0; # for output
param accum_value default 0;  # ibid.

var take{ITEMS} binary; # 1 if we take item i; 0 otherwise

# Objective: maximize total value
maximize MaxVal:
         sum{i in ITEMS} VALUE[i] * take[i];

#  Weight restriction
subject to Weight: 
        sum{i in ITEMS} WEIGHT[i] * take[i] <= WTMAX;


data;
# includes the data file
# include knapsack.dat; # original
include knapsack2.dat;  

# option cplex_options 'writeprob=xxx.lp';
option solver cplex;
# option lpsolve_options "printsol=7 prlp psols psolsa trace";
# option solver lpsolve;
solve;

# Print out the solution


# option omit_zero_rows 1;
display take;
display WTMAX, sum{i in ITEMS} VALUE[i] * take[i], sum{i in ITEMS} WEIGHT[i] * take[i];
print "Items to take:";
printf "Item\tWeight\tValue\t\tA. weight\tA. value\n";

for {i in ITEMS: take[i] > 0} {
     let accum_value := accum_value + VALUE[i];
     let accum_weight :=  accum_weight + WEIGHT[i];
     printf "%0s\t%3d\t%3d\t%3d\t%3d\n", i, WEIGHT[i], VALUE[i], accum_weight, accum_value;
};

printf "\n";

# expand;

quit;
 

