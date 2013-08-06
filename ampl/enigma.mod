/*


http://www.chlond.demon.co.uk/puzzles/sol4s7.html

model 'enigma'

! Description  : Enigma
! Source       : Herald Tribune circa November 1979 - courtesy of Dr Tito A. Ciriani  
! Date written : 12/8/2000
! Written by   : M J Chlond

  uses 'mmxprs'

O - 1
N - 7
E - 5
T - 6
W - 0
H - 8
R - 2
L - 3
V - 9
Y - 4

Giving:

ONE + ONE + TWO + TWO + THREE + ELEVEN = TWENTY
175 + 175 + 601 + 601 + 68255 + 535957 = 605764


Men jag får följande resultat:

CPLEX 10.1.0: optimal integer solution; objective 605764
17494 MIP simplex iterations
2393 branch-and-bound nodes

O: 4
N: 0
E: 8
T: 7
W: 9
H: 2
R: 3
L: 1
V: 5
Y: 6

[Using a Constraint Programming system, MiniZinc, the following 
 solutions are given:
y: [9, 1, 8, 6, 4, 7, 5, 2, 3, 0]
----------
y: [9, 1, 6, 8, 4, 7, 5, 2, 3, 0]
----------
y: [4, 0, 8, 7, 9, 2, 3, 1, 5, 6]   <--
----------
y: [4, 0, 6, 7, 9, 2, 3, 1, 5, 8]
----------
y: [0, 8, 6, 9, 4, 2, 3, 7, 1, 5]
----------
y: [0, 6, 8, 9, 4, 2, 3, 7, 1, 5]
]


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set N = 1..10;
var x{N,N} binary;

param letters{N} symbolic;

# x(i,j) = 1 if digit (i-1) represents letter j
# letter code 1 = O, 2 = N, 3 = E, 4 = T, 5 = W
#             6 = H, 7 = R, 8 = L, 9 = V,10 = Y
# example: x(10,10) = 1 means digit 9 is assigned to letter Y

minimize any:
        sum{i in N} 100000*(i-1)*x[i,4] + sum{i in N} 10000*(i-1)*x[i,5] + sum{i in N} 1000*(i-1)*x[i,3] +
        sum{i in N} 100*(i-1)*x[i,2] + sum{i in N} 10*(i-1)*x[i,4] + sum{i in N} (i-1)*x[i,10];

subject to dcon{i in N}:
        sum{j in N} x[i,j] = 1  # each digit assigned once
;
subject to lcon{j in N}:
        sum{i in N} x[i,j] = 1  #each letter assigned one digit
;

subject to con:
        sum{i in N} 100*(i-1)*x[i,1] + sum{i in N} 10*(i-1)*x[i,2] + sum{i in N} (i-1)*x[i,3] +
        sum{i in N} 100*(i-1)*x[i,1] + sum{i in N} 10*(i-1)*x[i,2] + sum{i in N} (i-1)*x[i,3] +
        sum{i in N} 100*(i-1)*x[i,4] + sum{i in N} 10*(i-1)*x[i,5] + sum{i in N} (i-1)*x[i,1] +
        sum{i in N} 100*(i-1)*x[i,4] + sum{i in N} 10*(i-1)*x[i,5] + sum{i in N} (i-1)*x[i,1] +
        sum{i in N} 10000*(i-1)*x[i,4] + sum{i in N} 1000*(i-1)*x[i,6] + sum{i in N} 100*(i-1)*x[i,7] +
        sum{i in N} 10*(i-1)*x[i,3] + sum{i in N} (i-1)*x[i,3] +
        sum{i in N} 100000*(i-1)*x[i,3] + sum{i in N} 10000*(i-1)*x[i,8] + sum{i in N} 1000*(i-1)*x[i,3] +
        sum{i in N} 100*(i-1)*x[i,9] + sum{i in N} 10*(i-1)*x[i,3] + sum{i in N} (i-1)*x[i,2] = 
        sum{i in N} 100000*(i-1)*x[i,4] + sum{i in N} 10000*(i-1)*x[i,5] + sum{i in N} 1000*(i-1)*x[i,3] + 
        sum{i in N} 100*(i-1)*x[i,2] + sum{i in N} 10*(i-1)*x[i,4] + sum{i in N} (i-1)*x[i,10]
;

data;

param letters :=
        1  "O", 
        2  "N", 
        3  "E", 
        4  "T", 
        5  "W"
        6  "H", 
        7  "R", 
        8  "L", 
        9  "V",
        10  "Y";

option solver gurobi;
# option solver cplex;
# option solver donlp2;
solve;

display x;

for{i in N} {
        printf "%s: ", letters[i];
        for{j in N: x[i,j] > 0.5} {
        printf "%d ", j-1;
        }
        printf "\n";
}

