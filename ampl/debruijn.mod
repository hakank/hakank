/*
  Tue Jan 15 07:15:23 2008/hakank@bonetmail.com

  (For TSP, see e.g.
   http://ite.pubs.informs.org/Vol7No1/LeeRaffensperger/
   )

  Testing to model de Bruijn sequences
  using TSP.

0 (0000 (0)) ->  12 (1100 (12)) [1.00]
1 (0001 (1)) ->  9 (1001 (9)) [1.00]
2 (0010 (2)) ->  3 (0011 (3)) [1.00]
3 (0011 (3)) ->  15 (1111 (15)) [1.00]
4 (0100 (4)) ->  13 (1101 (13))) [1.00]
5 (0101 (5)) ->  4 (0100 (4)) [1.00]
6 (0110 (6)) ->  14 (1110 (14)) [1.00]
7 (0111 (7)) ->  11 (1011 (11)) [1.00]
8 (1000 (8)) ->  2 (0010 (2)) [1.00]
9 (1001 (9)) ->  7 (0111 (7)) [1.00]
10 (1010 (10)) ->  8 (1000 (8)) [1.00]
11 (1011 (11)) ->  5 (0101 (5)) [1.00]
12 (1100 (12)) ->  10 (1010 (10)) [1.00]
13 (1101 (13))) ->  6 (0110 (6)) [1.00]
14 (1110 (14)) ->  0 (0000 (0)) [1.00]
15 (1111 (15)) ->  1 (0001 (1)) [1.00]

0 -> 12 
12 -> 10
10 -> 8
8 -> 2
2 -> 3
3 -> 15
15 -> 1
1 -> 9
9 -> 7
7 -> 11
11 -> 5
5 -> 4
4 -> 13
13 -> 6
6 -> 14
14 -> 0


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

set city;
# var U{city} >= 0 integer;
var U{city} >= 0 integer;
# param DIST{city, city} >= 0 default 10000;
set DIST within city cross city;
# var x{city,city} binary;
var x{DIST} binary;

param N := 5;#card(city); # hur lång?

param names{city} symbolic;

param temp;
param temp_count;

minimize z:
#        sum{i in city, j in city} DIST[i,j]*x[i,j];
        sum{ (i,j) in DIST} x[i,j];

# a citu has exact one outgoing
c1{k in city}: sum{i in city: (i,k) in DIST} x[i,k] <= 1;
#c1{ (k,l)  in DIST}: sum{ (i,j) in DIST} x[i,k] = 1;

# a city has exactly one incoming
c2{k in city}: sum{j in city: (k,j) in DIST} x[k,j] <= 1;
# c2{ (k,l)  in DIST}: sum{ (i,j) in DIST} x[k,j] = 1;

# new: exactly N arcs
c3: sum{ (i, j) in DIST } x[i,j] = N;

# no subtours (from tsp_winston.mod)
c4{ (j,k) in DIST: j > 0 and k > 0}:  
        U[k] - U[j] + N*x[j,k] <= N;



data;

param: city: names := 
        0  "0000 ( 0)"
        1  "0001 ( 1)"
        2  "0010 ( 2)"
        3  "0011 ( 3)"
        4  "0100 ( 4)"
        5  "0101 ( 5)"
        6  "0110 ( 6)"
        7  "0111 ( 7)"
        8  "1000 ( 8)"
        9  "1001 ( 9)"
        10 "1010 (10)"
        11 "1011 (11)"
        12 "1100 (12)"
        13 "1101 (13))"
        14 "1110 (14)"
        15 "1111 (15)"
;

/*
# (8,2)
# 0->0 and 7->7 are not interesting
set DIST := 
# 0 0 
0 1
1 2 
1 3
2 4 
2 5
3 6 
3 7
4 0 
4 1
5 2 
5 3
6 4 
6 5
7 6 
#7 7
;
*/


#/*
# For a (16,2) graph the connections are
set DIST :=
#0 0
0 1
1 2
1 3
2 4
2 5
3 6
3 7
4 8
4 9
5 10
5 11
6 12
6 13
7 14
7 15
8 0
8 1
9 2
9 3
10 4
10 5
11 6
11 7
12 8
12 9
13 10
13 11
14 12
14 13
15 14
#15 15
;
#*/

option presolve 0;
#option cplex_options "sensitivity writeprob=debruijn.lp";
option solver cplex;
# option solver minos;
#option solver cbc;
#option solver snopt;
# option solver gjh;
# option solver ipopt;
# option solver bonmin;
#option solver lpsolve;
# option solver donlp2;
# option solver loqo;

solve;

# printf "card(city): %d\n", card(city);
display N;
display DIST;
display z;
display x;
#display U;

#for{i in city} {
#  for{j in city: x[i,j] >= 0.02} {
for{ (i,j) in DIST: x[i,j] > 0} {
        printf "%2d %s  ->  %2d %s [%0.2f]\n", i,names[i], j, names[j], x[i,j];
        # printf "%d ->  %d (%0.2f)\n", i, j, x[i,j];
  # }
}


# a more intelligent loop
let temp := 0;
let temp_count := 0;
repeat {
        # printf "%d ", temp;
        for{ (temp,j) in DIST: x[temp,j] > 0} {
           printf "%2d -> %2d\n",temp,j, temp;
           let temp := j;
        }
        let temp_count := temp_count + 1;
        if temp = 0 or temp_count > card(DIST)  then break;
}

printf "It was %d nodes in the cycle.\n", temp_count;
printf "It was %d arcs\n", sum{ (i, j) in DIST } x[i,j];

# expand;
