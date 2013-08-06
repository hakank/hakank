/*

  Quasigroup completion in AMPL+CP.

  See 
  Carla P. Gomes and David Shmoys:
  "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"

  
  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  """
  Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
  a four-by-four array so that no column or row contains the same two numbers. 
  The result is known as a Latin square.
  ...
  The so-called quasigroup completion problem concerns a table that is correctly 
  but only partially filled in. The question is whether the remaining blanks in 
  the table can be filled in to obtain a complete Latin square (or a proper 
  quasigroup multiplication table).
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param m{1..n, 1..n} default 0;

var x{1..n, 1..n} >= 1 <= n integer;

#
# constraints
#
s.t. c1{i in 1..n, j in 1..n: m[i,j] > 0}:
        x[i,j] = m[i,j];

s.t. c2{i in 1..n}:
        alldiff{j in 1..n} x[i,j];

s.t. c3{j in 1..n}:
        alldiff{i in 1..n} x[i,j];

# data quasigroup_completion0.dat;
data quasigroup_completion_gomes_demo1.dat;
# data quasigroup_completion_gomes_demo2.dat;
# data quasigroup_completion_gomes_demo3.dat;
# data quasigroup_completion_gomes_demo4.dat; # (should give no solutions, and it does)
# data quasigroup_completion_gomes_demo5.dat;# (should give no solutions, and it does)
# data quasigroup_completion_gomes_shmoys_p3.dat;
# data quasigroup_completion_gomes_shmoys_p7.dat;
# data quasigroup_completion_martin_lynce.dat;

# data;

# # Example from Ruben Martins and Inès Lynce
# # Breaking Local Symmetries in Quasigroup Completion Problems, page 3
# # The solution is unique:
# #    1 3 2 5 4
# #    2 5 4 1 3
# #    4 1 3 2 5
# #    5 4 1 3 2
# #    3 2 5 4 1
# param n := 5;
# param m: 1 2 3 4 5 :=
#   1  1 . . . 4
#   2  . 5 . . .
#   3  4 . . 2 .
#   4  . 4 . . .
#   5  . . 5 . 1
# ;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=rnd outlev=1 outfreq=1';

solve;

for{i in 1..n} {
   for{j in 1..n} {
      printf "%2d ", x[i,j];
   }
   printf "\n";
}
printf "\n";