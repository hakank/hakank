/*

  Mrs Timpkin's Age problem in AMPL+CP.

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.

  When the Timpkinses married eighteen years ago, Timpkins was three
  times as old as his wife, and today he is just twice as old as she.
  How old is Mrs. Timpkin? 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var T >= 1 <= 100 integer;
var W >= 1 <= 100 integer;

#
# constraints
#
s.t. c1:
  T - 18 = 3 * (W - 18) and
  T = 2 * W

;

data;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=med outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display T,W;