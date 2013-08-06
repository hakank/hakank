/*

  Finding an optimal wedding seating chart in AMPL+CP.

  From 
  Meghan L. Bellows and J. D. Luc Peterson
  "Finding an optimal seating chart for a wedding"
  http://www.improbable.com/news/2012/Optimal-seating-chart.pdf
  http://www.improbable.com/2012/02/12/finding-an-optimal-seating-chart-for-a-wedding
  
  """
  Every year, millions of brides (not to mention their mothers, future 
  mothers-in-law, and occasionally grooms) struggle with one of the 
  most daunting tasks during the wedding-planning process: the 
  seating chart. The guest responses are in, banquet hall is booked, 
  menu choices have been made. You think the hard parts are over, 
  but you have yet to embark upon the biggest headache of them all. 
  In order to make this process easier, we present a mathematical 
  formulation that models the seating chart problem. This model can 
  be solved to find the optimal arrangement of guests at tables. 
  At the very least, it can provide a starting point and hopefully 
  minimize stress and arguments… 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n; # max number of tables
param a; # maximum number of guests a table can seat
param b; # minimum number of people each guest knows at their table
param m; # maximum number of guests

param C{1..m, 1..m};

# decision variables
var tables{1..m} >= 1 <= n integer;

var z >= 0 <= sum{j in 1..m, k in 1..m} C[j,k] integer; 

maximize obj: z;

#
# constraints
#

s.t. c1: z = sum{j in 1..m, k in 1..m: j < k}
          C[j,k]*(if tables[j]=tables[k] then 1)
;

s.t. c2a{i in 1..n}:
     count{j in 1..m, k in 1..m: j < k} (C[j,k] > 0 and
                                         tables[j] = i and
                                         tables[k] = i) >= b
;

s.t. c2b{i in 1..n}:
     sum{j in 1..m} (if tables[j] = i then 1) <= a
;
   
# symmetry breaking
s.t. c3: 
   # tables[3] = 1 # Martha sits at table 1
   tables[1] = 1   
;

data;

param m := 17;

param n := 5;
param a := 4;
param b := 2;

# slightly easier
# param n := 5;
# param a := 4;
# param b := 1;


# easier problem
# param n := 2;
# param a := 10;
# param b := 1;

 

param C: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 := 
 1  1 50  1  1  1  1  1  1  1  0  0  0  0  0  0  0  0 
 2 50  1  1  1  1  1  1  1  1  0  0  0  0  0  0  0  0 
 3  1  1  1 50  1  1  1  1 10  0  0  0  0  0  0  0  0 
 4  1  1 50  1  1  1  1  1  1  0  0  0  0  0  0  0  0 
 5  1  1  1  1  1 50  1  1  1  0  0  0  0  0  0  0  0 
 6  1  1  1  1 50  1  1  1  1  0  0  0  0  0  0  0  0 
 7  1  1  1  1  1  1  1 50  1  0  0  0  0  0  0  0  0 
 8  1  1  1  1  1  1 50  1  1  0  0  0  0  0  0  0  0 
 9  1  1 10  1  1  1  1  1  1  0  0  0  0  0  0  0  0 
10  0  0  0  0  0  0  0  0  0  1 50  1  1  1  1  1  1 
11  0  0  0  0  0  0  0  0  0 50  1  1  1  1  1  1  1 
12  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1 
13  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1 
14  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1 
15  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1 
16  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1 
17  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1
;




option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display tables;
display z;