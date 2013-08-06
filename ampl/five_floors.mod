/*

  Five floors problem in AMPL+CP.

  From Alexey Radul & Gerald Jay Sussman:
  "The Art of Propagator", page 34
  """
  Baker, Cooper, Fletcher, Miller, and Smith live on the first
  five floors of this apartment house. Baker does not live on the
  fifth floor. Cooper does not live on the first floor. Fletcher
  does not live on either the fifth or the first floor. Miller lives
  on a higher floor than does Cooper. Smith does not live on a
  floor adjacent to Fletcher'. Fletcher does not live on a floor
  adjacent to Cooper's.
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

set Persons;

# decision variables
var x{Persons} >= 1 <= n integer;

#
# constraints
#
s.t. c1: alldiff{i in Persons}  x[i];

s.t. c2:
    # Baker does not live on the fifth floor.
    x['Baker'] != 5 and

    # Cooper does not live on the first floor. 
    x['Cooper'] != 1 and

    # Fletcher does not live on either the fifth or the first floor. 
    x['Fletcher'] != 5 and x['Fletcher'] != 1 and

    # Miller lives on a higher floor than does Cooper. 
    x['Miller'] > x['Cooper'] and

    # Smith does not live on a floor adjacent to Fletcher'. 
    abs(x['Smith']-x['Fletcher']) > 1 and

    # Fletcher does not live on a floor adjacent to Cooper's.
    abs(x['Fletcher']-x['Cooper']) > 1
;

data;

param n := 5;

set Persons = Baker Cooper Fletcher Miller Smith;



# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=degree_max val_branching=max outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;



