/*

  Simple CP modeling in AMPL+CP.

  Marriott & Stuckey "Programming with Constraints", page 257.
 
  Balancing on a seesaw.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param d; # The distance

# decision variables
var Liz >= -5 <= 5 integer;
var Fi >= -5 <= 5 integer;
var Sara >= -5 <= 5 integer;


#
# constraints
#
s.t. c1: 9 * Liz + 8 * Fi + 4 * Sara = 0;
s.t. c2: Liz >= Fi + d;
s.t. c3: Liz >= Sara + d;
s.t. c4: Sara >= Fi + d;
# symmetry breaking
s.t. c5: Sara >= 0;
 

data;

param d := 3;

option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_max val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";


solve;


display Liz, Fi, Sara;