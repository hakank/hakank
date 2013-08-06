/*

  Magic squares in AMPL+CP.

  See 
   - http://en.wikipedia.org/wiki/Magic_square
   - http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob019/index.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

var x{1..n,1..n} >= 1 <= n*n integer;
var total >= 1 <= n*n*n integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n, j in 1..n} x[i,j];

s.t. c2{k in 1..n}:
     sum{i in 1..n} x[k,i] = total;

s.t. c3{k in 1..n}:
     sum{i in 1..n} x[i,k] = total;

# diagonal
s.t. d1: total = sum{i in 1..n} x[i,i];
s.t. d2: total = sum{i in 1..n} x[i,n-i+1];

s.t. c4: total = (n * (n*n + 1)) / 2;

data;

param n := 12;


option solver gecode;
option gecode_options 'var_branching=size_min val_branching=rnd outlev=1 outfreq=1';

solve;

# display x;
display total;

for{i in 1..n} {
   for {j in 1..n} {
     printf "%3d ", x[i,j];
   }
   printf "\n";
}
