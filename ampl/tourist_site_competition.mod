/*

  Tourist site competition in AMPL+CP.

  From Pierre Flener's presentation 
  "Constraint Technology - A Programming Paradigm on the Rise"
  http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
     pages 5f: problem statement 
     pages 12f: model
     pages 21ff: walktrough of a solution

  With 7 tourist sites and 7 judges:
  """
  Every tourist site is visited by r = 3 judges.
  Every judge visits c = 3 tourist sites.
  Every pair of sites is visited by lambda = 1 common judge.
  """

  There are 151200 solutions to this problem.
  With the additional constraint that Ali should visit Birka, Falun and Lund
  there are 4320 solutions.

  This problem was also presented as "The Airline-of-the-Year Problem"
  in his (Flener's) presentation
  "Constraint Programming - Programming Paradigm on the Rise"
  http://www.it.uu.se/research/group/astra/ATM-CT/Flener.pdf
  page 4f
  The problem is stated as follows for 7 airlines and 7 judges:
  """
  Constant jury: Every airline is tested by 3 judges.
  Constant load: Every judge tests 3 airlines.
  Equity: Every airline pair is tested by 1 common judge.
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param r;
param c;
param lambda;

param num_sites;
param num_judges;

# decision variables
var x{1..num_sites, 1..num_judges} binary;


#
# constraints
#

# Every tourist site is visited by r judges.
s.t. c1{s in 1..num_sites}:
  r = sum{j in 1..num_judges} (x[s,j])
;

# Every judge visits c tourist sites.
s.t. c2{j in 1..num_judges}:
  c = sum{s in 1..num_sites} (x[s,j])
;

# Every pair of sites is visited by lambda common judge.
s.t. c3{s1 in 1..num_sites, s2 in 1..num_sites: s1 < s2}:
  exactly lambda {j in 1..num_judges} (x[s1,j] = 1 and x[s1,j] = x[s2,j])
;

# Symmetry breaking
s.t. c4{s in 1..3}:
  x[s, 1] = 1
;



data;

param r := 3;
param c := 3;
param lambda := 1;
param num_sites := 7;
param num_judges := 7;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;
