#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Number of days problem (knapsack) in Z3
#
# From Nathan Brixius
# "Solving a Knapsack problem with Solver Foundation and LINQ"
# http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
# """
# Let's say I have this list of days and prices:
#
#    List<ReservationPrice> prices = new List<ReservationPrice>(); 
#    prices.Add(new ReservationPrice { NumberOfDays = 1, Price = 1000 }); 
#    prices.Add(new ReservationPrice { NumberOfDays = 2, Price = 1200 }); 
#    prices.Add(new ReservationPrice { NumberOfDays = 3, Price = 2500 }); 
#    prices.Add(new ReservationPrice { NumberOfDays = 4, Price = 3100 }); 
#    prices.Add(new ReservationPrice { NumberOfDays = 7, Price = 4000 }); 
#
# What I would like to able to do now is: give me the best price from the 
# list based on a number of days.
#
# So if ask for 3 days the best price from the list is from child one 
# (1000) and two (1200), but there are of course different combinations. 
# How would an algorithm that found the best price from this list look like ?
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

num_days = 5

target_days = 13

data = [
  [1,1000],
  [2,1200],
  [3,2500],
  [4,3100],
  [7,4000]
]

x = makeIntVector(sol,"x", num_days, 0,1)

days = makeIntVar(sol,"days",0,10000)

total_cost = makeIntVar(sol,"total_cost", 0,10000)

sol.add(total_cost == Sum([x[i]*data[i][1] for i in range(num_days)]))
sol.add(target_days == Sum([x[i]*data[i][0] for i in range(num_days)]))


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("target_days:", target_days)
  print("x :", [mod.eval(x[i]) for i in range(num_days)])
  print("total_cost  :", mod.eval(total_cost))
  print()
  getLessSolution(sol,mod,total_cost)

print("num_solutions:", num_solutions)



