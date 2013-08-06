/*
  Mon Dec 31 15:00:07 2007/hakank@bonetmail.com

  Winston OR, page 420


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set nodes;

# Winston's verbose version
var x{nodes, nodes} integer >= 0;
param capacities{nodes, nodes} integer >= 0;


maximize z:
        x["si", "so"];

subject to c_demand{i in nodes, j in nodes}:
        x[i,j] >= capacities[i,j];

subject to c_rows{i in nodes}:
        sum{j in nodes} x[i,j] >= 1;


subject to c_cols{j in nodes}:
        sum{i in nodes} x[i,j] >= 1;


data;

set nodes := "so" "1" "2" "3" "si";
param capacities: "so" "1" "2" "3" "si" :=
        "so"    0 2 3 0 0
        "1"     0 0 3 4 0
        "2"     0 0 0 0 2
        "3"     0 0 0 0 1
        "si"    0 0 0 0 0
;


#option cplex_option "sensitivity";
option solver cplex;
#option solver bonmin;
solve;
display z;
#display capacities;
#display x;

printf "capacity:\n";
for{i in nodes} {
  printf "%2s: ", i;
  for{j in nodes} {
    printf "%d ", capacities[i,j];
  }
  printf "\n";
}


printf "\nx:\n";
for{i in nodes} {
  printf "%2s: ", i;
  for{j in nodes} {
    printf "%d ", x[i,j];
  }
  printf "\n";
}

printf "\n";
for{i in nodes, j in nodes: x[i,j] > 0} {
        printf "%s->%s capacities:%d x:%d\n", i,j, capacities[i,j], x[i,j];
}
printf "\n";
