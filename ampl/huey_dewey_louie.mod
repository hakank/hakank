/*

  Huey, Dewey and Louie problem in AMPL+CP.

  From Marriott & Stuckey, Programming with Constraints, page 42
  """
  Huey, Dewey and Louie are being questioned by their uncle. These are the 
  statements the make:
   Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
   Dewey: If Huey is guilty, then so am I.
   Louie: Dewey and I are not both quilty.
  
  Their uncle, knowing that they are cub scouts, realises that they cannot tell a lie.
  Has he got sufficient information to decide who (if any) are quilty?
  """

  Solution:
    dewey = false
    huey = false
    louie = false
  i.e. no one is guitly.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var huey binary;
var dewey binary;
var louie binary;

#
# constraints
#
# Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
s.t. c1: dewey <==> louie;
  
# Dewey: If Huey is guilty, then so am I.
s.t. c2:  huey ==> dewey;

# Louie: Dewey and I are not both quilty.
s.t. c3: not (dewey and louie)
;

data;




# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1";


# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display huey, dewey, louie;


