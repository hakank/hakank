% 
% Number of days problem (knapsack) in MiniZinc.
% 
% From Nathan Brixius
% "Solving a Knapsack problem with Solver Foundation and LINQ"
% http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
% """
% Let's say I have this list of days and prices:
%
%    List<ReservationPrice> prices = new List<ReservationPrice>(); 
%    prices.Add(new ReservationPrice { NumberOfDays = 1, Price = 1000 }); 
%    prices.Add(new ReservationPrice { NumberOfDays = 2, Price = 1200 }); 
%    prices.Add(new ReservationPrice { NumberOfDays = 3, Price = 2500 }); 
%    prices.Add(new ReservationPrice { NumberOfDays = 4, Price = 3100 }); 
%    prices.Add(new ReservationPrice { NumberOfDays = 7, Price = 4000 }); 
%
% What I would like to able to do now is: give me the best price from the 
% list based on a number of days.
%
% So if ask for 3 days the best price from the list is from child one 
% (1000) and two (1200), but there are of course different combinations. 
% How would an algorithm that found the best price from this list look like ?
% """
%
% Compare with the ECLiPSe model:
% http://www.hakank.org/eclipse/number_of_days.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: num_days = 5;
array[1..num_days, 1..2] of int: data;

array[1..num_days] of var 0..1: x;
var int: days;
var int: total_cost;

solve minimize total_cost;

constraint
  total_cost = sum(i in 1..num_days) (
       x[i]*data[i,2]
  )
  /\
  days = sum(i in 1..num_days) (x[i]*data[i,1])
  /\
  days = 13
;


data = array2d(1..num_days,1..2,
[
  1,1000,
  2,1200,
  3,2500,
  4,3100,
  7,4000
]);


output
[ 
  "x: " ++ show(x) ++ "\n" ++ 
  "days: " ++ show(days) ++ "\n" ++ 
  "total_cost: " ++ show(total_cost) ++ "\n" 

];