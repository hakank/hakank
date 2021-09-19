"""
P-median problem in cpmpy.

Model and data from the OPL Manual, which describes the problem:
'''
The P-Median problem is a well known problem in Operations Research.
The problem can be stated very simply, like this: given a set of customers
with known amounts of demand, a set of candidate locations for warehouses,
and the distance between each pair of customer-warehouse, choose P
warehouses to open that minimize the demand-weighted distance of serving
all customers from those P warehouses.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def p_median():



  #
  # data
  #
  p = 2

  num_customers = 4
  customers = list(range(num_customers))
  Albert, Bob, Chris, Daniel = customers
  num_warehouses = 3
  warehouses = list(range(num_warehouses))
  Santa_Clara, San_Jose, Berkeley = warehouses

  demand = [100, 80, 80, 70]
  distance = [[2, 10, 50], [2, 10, 52], [50, 60, 3], [40, 60, 1]]

  #
  # declare variables
  #
  open = boolvar(shape=num_warehouses,name="open")
  ship = boolvar(shape=(num_customers,num_warehouses),name="ship")
  z = intvar(0, 1000, name="z")

  model = Model(minimize=z)

  #
  # constraints
  #
  model += [z == sum([ demand[c] * distance[c][w] * ship[c, w]
                       for c in customers
                       for w in warehouses])]

  for c in customers:
    # model += [sum([ship[c, w] for w in warehouses]) == 1]
    model += [sum(ship[c]) == 1]

  model += [sum(open) == p]

  for c in customers:
    for w in warehouses:
      model += [ship[c, w] <= open[w]]


  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("z:", z.value())
    print("open:", open.value())
    for c in customers:
      for w in warehouses:
        print(ship[c, w].value(), end=' ')
      print()
    print()

  print('num_solutions:', num_solutions)


p_median()
