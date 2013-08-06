/*

  Young tableaux in AMPL+CP.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}

  And the corresponding standard Young tableaux are:

  1.   1 2 3 4

  2.   1 2 3         1 2 4    1 3 4
       4             3        2

  3.   1 2           1 3
       3 4           2 4

  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3

  5.   1
       2
       3
       4
  """  
  


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;


var x{1..n, 1..n} >= 1 <= n+1 integer;
var p{1..n} >= 0 <= n+1 integer;

# i=1..n is used exactly once (n+1 may be used as much as possible)
s.t. c1{i in 1..n}:
     exactly 1 {a in 1..n, b in 1..n} (x[a,b] = i);

s.t. c2: x[1,1] = 1;

# row wise
s.t. c3{i in 1..n, j in 2..n}: x[i,j] >= x[i,j-1];

# column wise
s.t. c4{j in 1..n, i in 2..n}: x[i,j] >= x[i-1,j];

#
# calculate the structure (the partition)
#   MiniZinc: 
#     forall(i in 1..n) (
#       p[i] = sum(j in 1..n) (bool2int(x[i,j] <= n))
#     )
s.t. c5{i in 1..n}: 
  exists{k in 0..n} 
     k = p[i] and 
     exactly k {j in 1..n} (x[i,j] <= n); 

s.t. c6: n = sum{i in 1..n} p[i] ;

s.t. c7{i in 2..n}: p[i-1] >= p[i];

# Testing
# s.t. c8: p[1] = 4;
# s.t. c8: p[1] >= 2;
# s.t. c8: p[1] >= n;

data;

param n := 6;

option presolve 0;

option solver gecode;
option gecode_options "var_branching=degree_size_min val_branching=min outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

# write gtest;

printf "\nx:\n";
for{i in 1..n} {
     for{j in 1..n} {   
         if x[i,j] <= n then {
            printf "%d ", x[i,j];
         } else {
            printf "  ";
         }
     }
     printf "\n";
}

printf "\np: ";
for{i in 1..n} {
  printf "%d ", p[i];
}
printf "\n";

if solve_result = 'infeasible' then {
  printf "Infeasible!\n";
} else {
  printf "Feasible!\n";
}
