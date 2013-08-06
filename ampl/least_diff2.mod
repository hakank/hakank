/*

  Least diff problem:

  Minimise the difference
   10000*A +  1000*B + 100*C + 10*D + E - (10000*F +  1000*G + 100*H + 10*I + J)

  Least diff problem with the same principle as se send_more_money2.mod
(inspirerade av http://www.chlond.demon.co.uk/puzzles/sol4s7.html )


CPLEX 10.1.0: optimal integer solution; objective 247
2336 MIP simplex iterations
183 branch-and-bound nodes
x [*,*]
:    1   2   3   4   5   6   7   8   9  10    :=
1    0   1   0   0   0   0   0   0   0   0
2    0   0   1   0   0   0   0   0   0   0
3    0   0   0   1   0   0   0   0   0   0
4    0   0   0   0   1   0   0   0   0   0
5    0   0   0   0   0   1   0   0   0   0
6    1   0   0   0   0   0   0   0   0   0
7    0   0   0   0   0   0   0   0   0   1
8    0   0   0   0   0   0   0   0   1   0
9    0   0   0   0   0   0   0   1   0   0
10   0   0   0   0   0   0   1   0   0   0
;

S: 5
E: 0
N: 1
D: 2
M: 3
O: 4
R: 9
Y: 8
X: 7
Z: 6
]


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set N = 1..10; # columns (number) 
set M = 1..10; # rows (letters)
var x{N,M} binary;

# Translation table
#        1  "S", 
#        2  "E", 
#        3  "N", 
#        4  "D", 
#        5  "M"
#        6  "O", 
#        7  "R", 
#        8  "Y",
#        9  "X",
#        10 "Z"

param letters{M} symbolic;

# x(i,j) = 1 if digit (i-1) represents letter j
# ...
# example: x(10,10) = 1 means digit 9 is assigned to letter Y

# A B C D E - F G H I J
minimize any:
        (
        sum{i in N} 10000*(i-1)*x[i,1] + 
        sum{i in N}  1000*(i-1)*x[i,2] + 
        sum{i in N}   100*(i-1)*x[i,3] + 
        sum{i in N}    10*(i-1)*x[i,4] + 
        sum{i in N}     1*(i-1)*x[i,5] 
        )
        -
        (
        sum{i in N} 10000*(i-1)*x[i,6] + 
        sum{i in N}  1000*(i-1)*x[i,7] + 
        sum{i in N}   100*(i-1)*x[i,8] + 
        sum{i in N}    10*(i-1)*x[i,9] + 
        sum{i in N}     1*(i-1)*x[i,10] 
       )
;




subject to dcon{i in N}:
        sum{j in M} x[i,j] = 1  # each digit assigned once
;
subject to lcon{j in M}:
        sum{i in N} x[i,j] = 1  #each letter assigned one digit
;


# SEND + MORE
subject to con:
        (
        (
        sum{i in N} 10000*(i-1)*x[i,1] + 
        sum{i in N}  1000*(i-1)*x[i,2] + 
        sum{i in N}   100*(i-1)*x[i,3] + 
        sum{i in N}    10*(i-1)*x[i,4] +
        sum{i in N}     1*(i-1)*x[i,5] 
        ) 
        - 
        (
        sum{i in N} 10000*(i-1)*x[i,6] + 
        sum{i in N}  1000*(i-1)*x[i,7] + 
        sum{i in N}   100*(i-1)*x[i,8] + 
        sum{i in N}    10*(i-1)*x[i,9] +
        sum{i in N}     1*(i-1)*x[i,10] 

        )
        ) >= 1
;


data;

param letters :=
        1  "A", 
        2  "B", 
        3  "C", 
        4  "D", 
        5  "E"
        6  "F", 
        7  "G", 
        8  "H",
        9  "I",
        10 "J"
;


option solver cplex;
#option solver cbc;
solve;

display x;

print "Solution should be: [50123-49876=247]";

for{i in N} {
        printf "%s: ", letters[i];
        for{j in N: x[j,i] > 0} {
        printf "%d ", j-1;
        }
        printf "\n";
}


for{i in 1..5} {
        for{j in N: x[j,i] > 0} {
        printf "%d", j-1;
        }
}

printf " - ";
for{i in 6..10} {
        for{j in N: x[j,i] > 0} {
        printf "%d", j-1;
        }
}
printf " = %d \n", any;


display {i in 1..10, j in 1..10: x[j,i] > 0} (j-1);

printf "\n";