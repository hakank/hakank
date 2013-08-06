/*

  Spreadsheet problem in AMPL.

  From Krzysztof Apt "Principles of Constraint Programming" page 16ff. Spreadsheet.

  Cf Winston "Artificial Intelligence", 3rd edition, page 235 
  (not the same values though)

  Note: This is not a CP model. It's just interesting with the spreadsheet
  analogy...


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var B1 >= 0.0 <= 1000.0;
var B4 >= 0.0 <= 1000.0;
var B5 >= 0.0 <= 1000.0;
var C4 >= 0.0 <= 1000.0;
var C5 >= 0.0 <= 1000.0;
var D4 >= 0.0 <= 1000.0;
var D5 >= 0.0 <= 1000.0;
var E7 >= 0.0 <= 1000.0;
var E8 >= 0.0 <= 1000.0;


#
# constraints
#
s.t. c1: B1 = 0.17;
s.t. c2: B4 = 3.5;
s.t. c3: B5 = 1.7;
s.t. c4: C4 = 1.5;
s.t. c5: C5 = 4.5;
s.t. c6: D4 = B4 * C4;
s.t. c7: D5 = B5 * C5;
s.t. c8: E7 = D4 + D5;
s.t. c9: E8 = E7 * (1.0 + B1);


data;


option show_stats 2;
# option presolve 0;

# option solver gecode;
# option gecode_options "var_branching=size_max val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

# option solver cplex;


solve;


display B1,B4,B5,C4,C5,D4,D5,E7,E8;