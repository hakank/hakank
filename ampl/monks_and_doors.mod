/*

  Monks and doors problem in AMPL+CP.

  From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
  """
  There is a room with four doors and eight monks. One or more of
  the doors may be exit. Each monk is either telling a lie or the truth.
  
  The monks make the following statements:
  Monk 1: Door A is the exit.
  Monk 2: At least one of the doors B and C is the exit.
  Monk 3: Monk 1 and Monk 2 are telling the truth.
  Monk 4: Doors A and B are both exits.
  Monk 5: Doors A and B are both exits.
  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
  
  Which door is an exit no matter who is a liar and who is telling the
  truth.
  """
  
  Answer: Door A is an exit.
          And monks 1, 7, and 8 are telling the truth.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var A binary;
var B binary;
var C binary;
var D binary;

var M1 binary;
var M2 binary;
var M3 binary;
var M4 binary;
var M5 binary;
var M6 binary;
var M7 binary;
var M8 binary;


#
# constraints
#
# Monk 1: Door A is the exit.
s.t. c1: M1 <==> A;

# Monk 2: At least one of the doors B and C is the exit.
s.t. c2: M2 <==> (B + C) >= 1;

# Monk 3: Monk 1 and Monk 2 are telling the truth.
s.t. c3: M3 <==> (M1 and M2);

# Monk 4: Doors A and B are both exits.
s.t. c4: M4 <==> (A and B);

# Monk 5: Doors A and C are both exits.
s.t. c5: M5 <==> (A and C);

# Monk 6: Either Monk 4 or Monk 5 is telling the truth.
s.t. c6: M6 <==> (M4 or M5);

# Monk 7: If Monk 3 is telling the truth, so is Monk 6.
s.t. c7: M7 <==> (M3 ==> M6);

# Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
s.t. c8: M8 <==> ((M7 and M8) ==> M1);
  
# Exactly one door is an exit.
s.t. c9: A + B + C + D = 1;


data;  

option show_stats 2;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display A,B,C,D;
display M1,M2,M3,M4,M5,M6,M7,M8;

