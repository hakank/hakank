/*

First allocation

Lower floor:
             1  1  1
             1  0  1
             1  1  2

Upper floor:
             3  2  3
             2  0  1
             3  1  3

Second allocation

Lower floor:
             1  1  1
             1  0  3
             1  1  1

Upper floor:
             2  3  3
             3  0  1
             3  3  2
Xpress-Mosel Model

model 'pilgrim'

! Description  : The Riddle of the Pilgrims
! Source       : Dudeney, H.E., (1949), The Canterbury Puzzles, 7th ed., Thomas Nelson and Sons.   
! Date written : Mosel 19/4/03
! Written by   : M J Chlond

  uses 'mmxprs'

Xpress:
1 1 1 
1 0 1 
1 1 2 

3 2 3 
2 0 1 
3 1 3 

1 3 1 
1 0 1 
1 1 1 

2 1 3 
3 0 3 
3 3 2 

cplex:
1 1 2
1 0 1
1 1 1

3 1 3
2 0 1
3 2 3

1 2 1
1 0 2
1 1 1

3 3 1
2 0 3
3 2 3

glpk:
1 1 1
1 0 1
2 1 1

3 2 3
1 0 2
3 1 3

1 1 1
1 0 3
1 1 1

2 3 3
3 0 1
3 3 2

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set N := 1..2; # solutions
set F := 1..2; # floors
set R := 1..3; # rows
set C := 1..3; # columns
    
var x{N,F,R,C} integer >= 0; # number of pilgrims in solution n 
                             # on floor f, row r, column c
  
# any objective
minimize any: x[1,1,1,1];

# difference between solutions is 3 pilgrims
s.t.  num: sum{j in F,k in R,m in C} x[1,j,k,m] + 3 = 
                sum{j in F,k in R,m in C} x[2,j,k,m];

# twice as many pilgrims on second floor as first floor
s.t. flr{i in N}:
        sum{k in R,m in C} 2*x[i,1,k,m] = sum{k in R,m in C} x[i,2,k,m];

# eleven on first and third rows (i.e. front and back sides]
s.t. relev{i in N,k in R : k <> 2}:
        sum{j in F,m in C} x[i,j,k,m] = 11;

# eleven on first and third columnss (i.e. left and right sides]
s.t. celev{i in N,m in C : m <> 2}:
        sum{j in F,k in R} x[i,j,k,m] = 11;
  
s.t. xxx1 {i in N,j in F,k in R,m in C : k <> 2 or m <> 2}:
  # at least one pilgrim to a room
    x[i,j,k,m] >= 1;

s.t. xxx2 {i in N,j in F,k in R,m in C : k <> 2 or m <> 2}:
  # at most three pilgrims to a room
    x[i,j,k,m] <= 3;

# no pilgrims allocated to central celsl
s.t. nocentral{i in N,j in F}:
    x[i,j,2,2] = 0;

#option presolve 0;
#option solver cplex;
solve;

for{i in N} {
    for{j in F} {
      for{k in R} {
        for{m in C} {
          printf "%d ", x[i,j,k,m];
      }
      printf "\n";
   }
  printf "\n";
  }

}



