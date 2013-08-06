/*

  Fractions problem in AMPL+CP.

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """

  There are 8 solutions:
    7, 2, 4, 5, 8, 9, 1, 3, 6
    7, 2, 4, 5, 8, 9, 1, 6, 3
    7, 2, 4, 5, 9, 8, 1, 3, 6
    7, 2, 4, 5, 9, 8, 1, 6, 3
    7, 4, 2, 5, 8, 9, 1, 3, 6
    7, 4, 2, 5, 8, 9, 1, 6, 3
    7, 4, 2, 5, 9, 8, 1, 3, 6
    7, 4, 2, 5, 9, 8, 1, 6, 3


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var A >= 1 <= n integer;
var B >= 1 <= n integer;
var C >= 1 <= n integer;
var D >= 1 <= n integer;
var E >= 1 <= n integer;
var F >= 1 <= n integer;
var G >= 1 <= n integer;
var H >= 1 <= n integer;
var I >= 1 <= n integer;

var x{1..n} >= 1 <= n integer;

var D1 >= 1 <= 81 integer;
var D2 >= 1 <= 81 integer;
var D3 >= 1 <= 81 integer;

#
# constraints
#
s.t. c0: 
   x[1] = A and
   x[2] = B and
   x[3] = C and
   x[4] = D and
   x[5] = E and
   x[6] = F and
   x[7] = G and
   x[8] = H and
   x[9] = I 
;
s.t. c1: alldiff{i in 1..n}  x[i];

s.t. c2:

   D1 = B*C and
   D2 = E*F and
   D3 = H*I and
   A*D2*D3 + D*D1*D3 + G*D1*D2 = D1*D2*D3 and
   # symmetry breaking
   A*D2 >= D*D1 and
   D*D3 >= G*D2 and
   # redundant constraints
   3*A >= D1 and
   3*G <= D2
;

data;

param n := 9;



# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=degree_max val_branching=max outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;



