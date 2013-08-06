/*

Set packing is the reverse of set covering (see Williams, page 179).

Using the example from set_covering3.mod (Murty)

Here we set the constraint to <= (instead of >= 1) and
maximizes the number of senators.

If the constraint is = 1, then it's set partitioning.

Set packing for this example yields:

x [*] :=
 1  1
 2  0
 3  0
 4  0
 5  0
 6  0
 7  0
 8  0
 9  1
10  0
;

I.e. Senators 1 and 9 should be in the committe.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set senators;
set group;
param belongs{group, senators} default 0;

var x{senators} binary;


maximize z: sum{i in senators} x[i];
#minimize z: sum{i in senators} x[i];

s.t. c1{i in group}: sum{j in senators} x[j]*belongs[i,j] <= 1;


data;

set group := southern northern liberals conservative democrats repulicans;

set senators :=      1 2 3 4 5 6 7 8 9 10;

param belongs:       1 2 3 4 5 6 7 8 9 10 :=
        southern     1 1 1 1 1 . . . . .
        northern     . . . . . 1 1 1 1 1
        liberals     . 1 1 . . . . 1 1 1
        conservative 1 . . . 1 1 1 . . . 
        democrats    . . 1 1 1 1 1 . 1 .
        repulicans   1 1 . . . . . 1 . 1
; 

option solver cplex;
solve;

display x;

for{i in senators: x[i] > 0} {
    printf "Senator %d. Represents: \n", i;
    for{j in group: belongs[j,i] > 0} {
      printf "\t%s\n", j;
    }
   printf "\n";
}
