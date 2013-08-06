/*

  Mama's age problem in AMPL+CP.

  Mamma's Age from "Amusements in Mathematics, Dudeney", number 40.
  """
  Tommy: "How old are you, mamma?"
  Mamma: "Our three ages add up to exactly seventy years."
  Tommy: "And how old are you, papa?"
  Papa: "Just six times as old as you, my son."
  Tommy: "Shall I ever be half as old as you, papa?"
  Papa: "Yes, Tommy; and when that happens our three ages will add up to
  exactly twice as much as today."

  Can you find the age of Mamma?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables

# in months
var m >= 1 <= 12000 integer; # mamma's age 
var p >= 1 <= 12000 integer; # papa's age
var t >= 1 <= 12000 integer; # tommy's age
var i >= 1 <= 12000 integer; # temp

# the real ages (years)
var m2 >= 1 <= 1000 integer; # mamma's age
var p2 >= 1 <= 1000 integer; # papa's age
var t2 >= 1 <= 1000 integer; # tommy's age

#
# constraints
#
s.t. c1:
   m + p + t = 70 * 12 and
   6 * t = p and
   (t + i) * 2 = p + i and
   m + i + p + i + t + i = 2 * (m + p + t)

   and
   # age in years
   m2 = m div 12 and
   p2 = p div 12 and
   t2 = t div 12
;



data;  

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

display m,p,t,i;
display m2,p2,t2;

