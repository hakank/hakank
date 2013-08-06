/*

  Social golfers problem in AMPL+CP.

  Translated from OPL code from

  http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=010
  """
  Problem description
  In a golf club there are 32 social golfers who play once a week in 8 groups of 4.  
  The problem amounts to find a schedule for as many as possible weeks, such that no 
  two golfers play in the same group more than once.

  Here we consider the decisional version of the problem (wrt the number of 
  weeks 'weeks'), where the number of players and the group size are given as input.
  
  Problem input
  
  * groups, the number of groups to be formed each week
  * groupSize, the size of each group
  * weeks, the number of weeks for which a scheduling is requested 
  
  Search space
  The set of all possible group assignments to all players in each of the weeks weeks.
  
  Constraints

  * C1: Each group has exactly groupSize players
  * C2: Each pair of players only meets at most once
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param weeks;     # number of weeks
param groups;    # number of groups (per week)
param groupSize; # group size
param golfers = groups * groupSize; # number of golfers

# decision variables
var x{1..golfers, 1..weeks} >= 1 <= groups integer;

#
# constraints
#
# C1: Each group has exactly groupSize players.
s.t. c1{gr in 1..groups, w in 1..weeks}:
  groupSize = count{g in 1..golfers} (if x[g,w] = gr then 1)
;

# C2: Each pair of players only meets at most once.
s.t. c2{g1 in 1..golfers, g2 in 1..golfers, 
        w1 in 1..weeks, w2 in 1..weeks: g1 != g2 and w1 != w2}:
  (if x[g1,w1] = x[g2,w1] then 1) + (if x[g1,w2] = x[g2,w2] then 1) <= 1
;

# SBSA: Symmetry-breaking by selective assignment.
# On the first week, the first groupSize golfers play in group 1, the 
# second groupSize golfers play in group 2, etc. On the second week, 
# golfer 1 plays in group 1, golfer 2 plays in group 2, etc.
s.t. c3{g in 1..golfers}:
  x[g,1]=((g-1) div groupSize) + 1 
;

s.t. c4{g in 1..golfers: g <= groupSize}: x[g,2]=g;


data;

param weeks := 4;
param groups := 4;
param groupSize := 4;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_max val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;
