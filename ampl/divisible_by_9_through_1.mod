/*

  Divisible by 9 through 1 problem in AMPL+CP.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  "Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/

  This is a slighly more general model by supporting
  different bases.

  
  For base <= 10 there are solution (i.e. the array x) 
  for the following
    2: [1]
    4: [1, 2, 3] 
       [3, 2, 1]
    6: [1, 4, 3, 2, 5] 
       [5, 4, 3, 2, 1]
    8: [3, 2, 5, 4, 1, 6, 7]
       [5, 2, 3, 4, 7, 6, 1]
       [5, 6, 7, 4, 3, 2, 1]
   10: [3, 8, 1, 6, 5, 4, 7, 2, 9]

  The ECLiPSe model 
     http://www.hakank.org/eclipse/divisible_by_9_trough_1.ecl
  also finds the following solution in base 14
   14: [9, 12, 3, 10, 5, 4, 7, 6, 11, 8, 1, 2, 13]



  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param base;
param n = base-1;
param m = ceil(base^n)-1; # 999999999 (for base 10)


# decision variables
var x{1..n} >= 1 <= n integer;
var t{1..n} >= 0 <= m integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];

s.t. c2{i in 1..n}:
  t[i] = sum{j in 1..base-i} ceil(base^(base-i-j))*x[j]
  and
  (t[base-i] mod i = 0)
;

data;

# solve this for bases 2, 4, 6, 8, and 10
# (base 12 give integer overflow)
for{i in 1..5} {
   let base := i*2;

   option solver gecode;
   option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

   # option solver ilogcp;
   # option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

   solve;

   display base,x,t;
}
