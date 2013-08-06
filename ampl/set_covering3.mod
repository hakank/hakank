/*

Set covering

From
http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
Katta G. Murty: "Optimization Models for Decision Making", page 302f

10 senators to make a committee with at least one representative from
each group. Minimize the number of senators in the committee.

group     senators
southern      1 2 3 4 5
northern      6 7 8 9 10
liberals      2 3 8 9 10
conservative  1 5 6 7
democrats     3 4 5 6 7 9
republicans   1 2 8 10

cplex:
x [*] :=
 1  0
 2  1
 3  0
 4  0
 5  0
 6  1
 7  0
 8  0
 9  0
10  0
;

Senator 2 should be in the committe. Represents:
        southern
        liberals
        repulicans

Senator 6 should be in the committe. Represents:
        northern
        conservative
        democrats

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set senators;
set group;
param belongs{group, senators} default 0;

var x{senators} binary;


minimize z: sum{i in senators} x[i];

s.t. c1{i in group}: sum{j in senators} x[j]*belongs[i,j] >= 1;


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
    printf "Senator %d should be in the committe. Represents: \n", i;
    for{j in group: belongs[j,i] > 0} {
      printf "\t%s\n", j;
    }
   printf "\n";
}
