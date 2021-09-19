"""
Number of days problem (knapsack) in cpmpy.

From Nathan Brixius
'Solving a Knapsack problem with Solver Foundation and LINQ'
http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
'''
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
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def number_of_days(target_days=13):
 
  num_days = 5


  data = np.array([[1,1000],
                   [2,1200],
                   [3,2500],
                   [4,3100],
                   [7,4000]
                   ])
  
  x = boolvar(shape=num_days, name="x")
  total_cost = intvar(0,sum(data[:,1]),name="total_cost")

  model = Model([target_days == sum([x[i]*data[i][0] for i in range(num_days)]),
                 total_cost == sum([x[i]*data[i][1] for i in range(num_days)])
                ],
                minimize=total_cost)


  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("x :", x.value())
    print("target_days (given)  :", target_days)
    print("total_cost:", total_cost.value())
    print()

  # print("num_solutions:", num_solutions)

target_days = 13
number_of_days(target_days)

for target_days in range(1,20):
  print("target_days:",target_days)
  number_of_days(target_days)  




