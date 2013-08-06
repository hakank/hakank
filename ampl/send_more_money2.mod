/*

SEND+MORE=MONEY

Based on enigma.mod
http://www.chlond.demon.co.uk/puzzles/sol4s7.html

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set N := 1..10; # columns (numbers) 
set M := 1..10; # rows (letters)
var x{N,M} binary;


param letters{M} symbolic;

# x(i,j) = 1 if digit (i-1) represents letter j
# ...
# example: x(10,10) = 1 means digit 9 is assigned to letter Y

minimize any:
(
        sum{i in N} 10000*(i-1)*x[i,5] + 
        sum{i in N}  1000*(i-1)*x[i,6] + 
        sum{i in N}   100*(i-1)*x[i,3] + 
        sum{i in N}    10*(i-1)*x[i,2] + 
        sum{i in N}     1*(i-1)*x[i,8] + 

        # hack
        sum{i in N} 0*(i-1)*x[i,9] + 
        sum{i in N} 0*(i-1)*x[i,10] 
)
-

        (
        sum{i in N} 1000*(i-1)*x[i,1] + 
        sum{i in N}  100*(i-1)*x[i,2] + 
        sum{i in N}   10*(i-1)*x[i,3] + 
        sum{i in N}    1*(i-1)*x[i,4] +

        sum{i in N} 1000*(i-1)*x[i,5] + 
        sum{i in N}  100*(i-1)*x[i,6] + 
        sum{i in N}   10*(i-1)*x[i,7] + 
        sum{i in N}    1*(i-1)*x[i,2] 
        ) 
;


subject to dcon{i in N}:
        sum{j in M} x[i,j] = 1  # each digit assigned once
;
subject to lcon{j in M}:
        sum{i in N} x[i,j] = 1  #each letter assigned one digit
;

# S > 0
subject to notnull1:
        sum{i in 2..10} x[i,1] >= 1;

# M > 0
subject to notnull2:
        sum{i in 2..10} x[i,5] >= 1;


# SEND + MORE
subject to con:
        sum{i in N} 1000*(i-1)*x[i,1] + 
        sum{i in N}  100*(i-1)*x[i,2] + 
        sum{i in N}   10*(i-1)*x[i,3] + 
        sum{i in N}    1*(i-1)*x[i,4] +

        sum{i in N} 1000*(i-1)*x[i,5] + 
        sum{i in N}  100*(i-1)*x[i,6] + 
        sum{i in N}   10*(i-1)*x[i,7] + 
        sum{i in N}    1*(i-1)*x[i,2] 

        -
(
        sum{i in N} 10000*(i-1)*x[i,5] + 
        sum{i in N}  1000*(i-1)*x[i,6] + 
        sum{i in N}   100*(i-1)*x[i,3] + 
        sum{i in N}    10*(i-1)*x[i,2] + 
        sum{i in N}     1*(i-1)*x[i,8]
)

        <= 0
;


       
#/*
data;

param letters :=
        1  "S", 
        2  "E", 
        3  "N", 
        4  "D", 
        5  "M"
        6  "O", 
        7  "R", 
        8  "Y",
        9  "X",
        10 "Z"
;
#*/

option cplex_options 'writeprob=send_more_money2.lp';
option solver cplex; # correct and fast
#option lpsolve_options "printsol=7 prlp psols psolsa trace";
#option solver lpsolve;
# option solver lpsolve; # correct and fast
# option solver LaGO; # correct and fast
# option solver bonmin; # yields Infeasible problem
# option solver ipopt; # not integer solution
# option solver cbc; # correct but slower than cplex
solve;

display x;

printf "Result:";
for{i in N} {
        printf "%s:", letters[i];
        for{j in N: x[j,i] > 0.5} {
        printf "%d ", j-1;
        }
        # printf "\n";
}
printf "\n";


# SEND
printf "SEND (%d) + ", sum{i in N} 1000*(i-1)*x[i,1] + 
        sum{i in N}  100*(i-1)*x[i,2] + 
        sum{i in N}   10*(i-1)*x[i,3] + 
        sum{i in N}    1*(i-1)*x[i,4];

# MORE
printf "MORE (%d) = ", sum{i in N} 1000*(i-1)*x[i,5] + 
        sum{i in N}  100*(i-1)*x[i,6] + 
        sum{i in N}   10*(i-1)*x[i,7] + 
        sum{i in N}    1*(i-1)*x[i,2];

# behöver inte ha hacket med x[,9] resp. x[,10]
printf "MONEY (%d)\n",
        sum{i in N} 10000*(i-1)*x[i,5] + 
        sum{i in N}  1000*(i-1)*x[i,6] + 
        sum{i in N}   100*(i-1)*x[i,3] + 
        sum{i in N}    10*(i-1)*x[i,2] + 
        sum{i in N}     1*(i-1)*x[i,8]# + 

        # hack
        # sum{i in N} 0*(i-1)*x[i,9] + 
        # sum{i in N} 0*(i-1)*x[i,10] 
;


# SEND MORE - MONEY
printf "Diff: %d\n",
        sum{i in N} 1000*(i-1)*x[i,1] + 
        sum{i in N}  100*(i-1)*x[i,2] + 
        sum{i in N}   10*(i-1)*x[i,3] + 
        sum{i in N}    1*(i-1)*x[i,4]
        + 
        sum{i in N} 1000*(i-1)*x[i,5] + 
        sum{i in N}  100*(i-1)*x[i,6] + 
        sum{i in N}   10*(i-1)*x[i,7] + 
        sum{i in N}    1*(i-1)*x[i,2]
        - (
        sum{i in N} 10000*(i-1)*x[i,5] + 
        sum{i in N}  1000*(i-1)*x[i,6] + 
        sum{i in N}   100*(i-1)*x[i,3] + 
        sum{i in N}    10*(i-1)*x[i,2] + 
        sum{i in N}     1*(i-1)*x[i,8])
;

/*
data;

param letters :=
        1  "S", 
        2  "E", 
        3  "N", 
        4  "D", 
        5  "M"
        6  "O", 
        7  "R", 
        8  "Y",
        9  "X",
        10 "Z"
;
*/
