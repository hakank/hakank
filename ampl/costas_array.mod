/*

  Costas array in AMPL+CP.

  Note: This model is based on Barry O'Sullivan's 
  MiniZinc model in the G12 repository:
  http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn


  From http://mathworld.wolfram.com/CostasArray.html:
  """
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  """

  Also see
  http://en.wikipedia.org/wiki/Costas_array


  About this model (comment from my MiniZinc model 
  http://www.hakank.org/minizinc/costas_array.mzn)
  As mentioned above this model is based on Barry O'Sullivan's 
  model. Here are the two rather simple differences 
  (marked by "hakank" below)
   1) no symmetry breaking on the order of the Costas array
   2) fixes the lower triangular matrix in the difference
      matrix to -n+1
  
  Since there is no symmetry breaking of the order of the Costas 
  array it gives all the solutions for a specific length of 
  the array, e.g. those 
  listed in http://mathworld.wolfram.com/CostasArray.html
  
  1	1	(1)
  2	2	(1, 2), (2,1)
  3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
  4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
                (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
                (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
  ....
  
  See http://www.research.att.com/~njas/sequences/A008404
  for the number of solutions for n=1..
  1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
  17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
  872, 200, 88, 56, 204,...
  


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var differences{1..n, 1..n} >= -n+1 <= n-1 integer;
var costas{1..n} >= 1 <= n integer;


#
# constraints
#


# 1) I skipped this constraint since I want 
#    to generate all solutions.
# s.t. c1: costas[1] < costas[n];

# 2) Fix the values in the lower triangle in the
# difference matrix to -n+1. This removes variants 
# of the difference matrix for the the same Costas array.
s.t. c2{i in 1..n, j in 1..i}: differences[i,j] = -n+1;


# hakank: All the following constraints are from 
# Barry O'Sullivans's original model.
s.t. c3: alldiff{i in 1..n} costas[i];


# "How do the positions in the Costas array relate 
#  to the elements of the distance triangle."
s.t. c4{i in 1..n,j in 1..n: i < j}:
   differences[i,j] = costas[j] - costas[j-i]
;
	

# "All entries in a particular row of the difference 
#  triangle must be distint."
s.t. c5{i in 1..n-1}:
   alldiff{j in 1..n: j > i} (differences[i,j])
;

# "All the following are redundant - only here to speed up search."

# "We can never place a 'token' in the same row as any other."
s.t. c6{i in 1..n,j in 1..n: i < j}:
   differences[i,j] != 0
;

s.t. c7{k in 3..n,l in 3..n: k < l}:
   differences[k-2,l-1] + differences[k,l] = differences[k-1,l-1] + differences[k-1,l]
;

data;

param n := 12;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display costas;
# display differences;

printf "costas: ";
for{i in 1..n} printf "%d, ", costas[i];
printf "\n";
for{i in 1..n} {
   for{j in 1..n} {
      if differences[i,j] > -n+1 then
        printf "%4d", differences[i,j];
      else 
        printf "  ";
   }
   printf "\n";
}