/*

  Number of days problem in AMPL+CP.

  From Nathan Brixius
  "Solving a Knapsack problem with Solver Foundation and LINQ"
  http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
  """
  Let's say I have this list of days and prices:

     List<ReservationPrice> prices = new List<ReservationPrice>(); 
     prices.Add(new ReservationPrice { NumberOfDays = 1, Price = 1000 }); 
     prices.Add(new ReservationPrice { NumberOfDays = 2, Price = 1200 }); 
     prices.Add(new ReservationPrice { NumberOfDays = 3, Price = 2500 }); 
     prices.Add(new ReservationPrice { NumberOfDays = 4, Price = 3100 }); 
     prices.Add(new ReservationPrice { NumberOfDays = 7, Price = 4000 }); 

  What I would like to able to do now is: give me the best price from the 
  list based on a number of days.

  So if ask for 3 days the best price from the list is from child one 
  (1000) and two (1200), but there are of course different combinations. 
  How would an algorithm that found the best price from this list look like ?
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param num_days;
param m{1..num_days, 1..2};
param days;


# decision variables
var x{1..num_days} binary;
var total_cost >= 0 integer;

minimize obj: total_cost;

#
# constraints
#
s.t. c1: total_cost = sum{i in 1..num_days} x[i]*m[i,2];
s.t. c2: days = sum{i in 1..num_days} x[i]*m[i,1];

data;  

param days := 13;
param num_days := 5;
param m: 1 2 :=
  1 1 1000
  2 2 1200
  3 3 2500
  4 4 3100
  5 7 4000
;



option show_stats 2;


option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display x;
display days;
display total_cost;
