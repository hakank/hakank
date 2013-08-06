/*

  Halmo's handshaking problem in AMPL+CP.

  Problem formulation from Alloy (examples/puzzles/handshake)
  """
  Alloy model of the Halmos handshake problem
  
  Hilary and Jocelyn are married. They invite four couples who are friends for dinner. When
  they arrive, they shake hands with each other. Nobody shakes hands with him or herself
  or with his or her spouse. After there has been some handshaking, Jocelyn jumps up on
  a chair and says "Stop shaking hands!", and then asks how many hands each person has
  shaken. All the answers are different. How many hands has Hilary shaken?
  
  The Alloy model represents the problem as a set of constraints. Properties of the spouse
  relationship and of handshaking in general are given as facts. The particular situation
  is cast as a function.
  
  There are 9 people answering, and all answers are different. Nobody can shake more than
  8 hands. So answers must be 0..8. The one (p8 say) who answered 8 has shaken everybody's
  hand except for his or her own, and his or her spouse's. Now consider the person who shook
  0 hands (p0 say). The persons p0 and p8 are distinct. If they are not married, then p8 cannot
  have shaken 8 hands, because he or she did not shake the hand of p0 or of his or her spouse.
  So p8's spouse to p0. Now imagine Jocelyn asking the question again, with p0 and p8 out of
  the room, and excluding hand shakes with them. Since p8 shook hands with everyone else
  except p0 and p8, everyone gives an answer one smaller than they did before, giving 0..6.
  The argument now applies recursively. So Hilary is left alone, having shaken 4 hands. 
  """
  Alloy is here: http://alloy.mit.edu/alloy
  
  Also, see the following that discuss Halmos' Handshake problem
  http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/mathhome.htm#halmos
      http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/shakeanswer.htm
  
  The origin of the problem seems to be
  P.R. Halmos: "To Count or to Think, That is the Question", page 1ff
  http://bernoulli.math.rug.nl/vorigelezingen/lezing03/lezing03.pdf


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n; # n / 2 pairs

check: n mod 2 = 0;

# decision variables

#    coded Pair1a,Pair1b, Pair2a,Pair2b
var x{1..n} >= 0 <= n-2 integer;

# who shake hands with whom:
#  (restrictions: not him/herself and not his/her spouse)
var y{1..n,1..n} binary;


#
# constraints
#

# We assume that Hilary is in position x[1]
# (and Hilary's spouse - Jocelyn - in x[2])
# All except Hilary's counts are different
s.t. c1: alldiff{i in 2..n} x[i];

# don't shake hand with spouse
s.t. c2{i in 0..(n div 2)-1}:
  y[2*i+1,2*i+2] = 0
  and
  y[2*i+2,2*i+1] = 0 
;

# don't shake hand with oneself
s.t. c3{i in 1..n}:
  y[i,i] = 0
  and
  # how many hands has x[i] shaken
  x[i] = sum{j in 1..n} y[i,j]
;

# symmetry of handshaking:
#    a shake hands with b <-> b shake hands with a
s.t. c4{i in 1..n, j in 1..n}:
  y[i,j] = 1 <==> y[j,i] = 1
;

# Symmetry breaking, which orders the other couples (besides the hosts)
# Without it: 384 solutions for n=10 (all x = [4,4,.....])
# With it: 1 solution: x: [4, 4, 0, 8, 1, 7, 2, 6, 3, 5] (since we order 0,1,2,3 shakes)
# 
# Note that all number of handshaking of the pairs sums to 8, i.e. 4+4, 0+8, 1+7, 2+6, 3+5
# More general: The number of handshaking per pair sums to n-2.
# 
#   MiniZinc: increasing([x[3+2*i] | i in 0..(n div 2)-2])
s.t. c5{i in 0..(n div 2)-2, j in 0..(n div 2)-2: i < j}:
   x[3+2*i] < x[3+2*j] 
;

# Same as c5 but "expanded" (for n=10)
# s.t. c5b: 
#    x[3] <= x[5] and
#    x[5] <= x[7] and
#    x[7] <= x[9]
#  ;
 
s.t. c6{i in 0..(n div 2)-2}:
  x[3+2*i] < x[3+2*i+1]
;

# Further symmetry breaking. The two first (the hosts) 
# have the same value: (n/2)-1
# s.t. c7: x[1] = (n / 2) -1 and x[2] = x[1];

# All pairs sums to (n/2)-1.
# s.t. c8{i in 1..n-1: n mod 2 = 1}: x[i] + x[i+1] = (n/2)-1;


# Just for testing contradictions
# s.t. c9: x[1] != 4
#          or x[2] != 4
# ;


data;

param n := 10;


option show_stats 2;

option solver gecode;
# option gecode_options "icl=def var_branching=afc_max val_branching=med outlev=1 outfreq=1";
option gecode_options "icl=def var_branching=regret_max_max val_branching=med outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

display x, y;
