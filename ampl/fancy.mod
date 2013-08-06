/*

  Mr Greenguest puzzle (fancy dress) in AMPL+CP.

  Problem (and LPL) code in
  http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
  """
  (** Mr. Greenfan wants to give a dress party where the male guests
   * must wear green dresses. The following rules are given:
   * 1 If someone wears a green tie he has to wear a green shirt.
   * 2 A guest may only wear green socks and a green shirt 
   *   if he wears a green tie or a green hat.
   * 3 A guest wearing a green shirt or a green hat or who does
   *   not wear green socks must wear a green tie.
   * 4 A guest who is not dressed according to rules 1-3 must
   *   pay a $11 entrance fee.
   * Mr Greenguest wants to participate but owns only a green shirt 
   * (otherwise he would have to pay one for $9). He could buy 
   * a green tie for $10, a green hat (used) for $2 and green socks
   * for $12.
   * What is the cheapest solution for Mr Greenguest to participate?
   *)
  """

  For an IP solution of this problem, see 
    http://www.hakank.org/ampl/dress_party.mod

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var t binary;
var h binary;
var r binary;
var s binary;
var n binary;

var cost >= 0 integer;

minimize obj: cost;

#
# constraints
#
s.t. c1:
  # This is a translation from the LPL code
  ( (t==>r) or n) and
  ( ((s or r) ==> (t or h)) or n )  and
  ( ((r or h or not s) ==> t) or n ) 
;

s.t. c2: cost = 10*t + 2*h + 12*s + 11*n;

data;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=degree_max val_branching=max outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display t,h,r,s,n, cost;



