# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""

  P-median problem in OR-tools CP-SAT Solver.

  Model and data from the OPL Manual, which describes the problem:
  '''
  The P-Median problem is a well known problem in Operations Research.
  The problem can be stated very simply, like this: given a set of customers
  with known amounts of demand, a set of candidate locations for warehouses,
  and the distance between each pair of customer-warehouse, choose P
  warehouses to open that minimize the demand-weighted distance of serving
  all customers from those P warehouses.
  '''

  This is port of my old CP model p_median.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main():

  model = cp.CpModel()

  #
  # data
  #
  p = 2

  num_customers = 4
  customers = list(range(num_customers))
  # Albert, Bob, Chris, Daniel = customers
  num_warehouses = 3
  warehouses = list(range(num_warehouses))
  # Santa_Clara, San_Jose, Berkeley = warehouses

  demand = [100, 80, 80, 70]
  distance = [[2, 10, 50], 
              [2, 10, 52], 
              [50, 60, 3], 
              [40, 60, 1]]

  #
  # declare variables
  #
  open = [model.NewIntVar(0,num_warehouses, 'open[%i]% % i') for w in warehouses]
  ship = {}
  for c in customers:
    for w in warehouses:
      ship[c, w] = model.NewIntVar(0, 1, 'ship[%i,%i]' % (c, w))

  z = model.NewIntVar(0, 1000, 'z')

  #
  # constraints
  #
  model.Add(z == sum([
      demand[c] * distance[c][w] * ship[c, w]
      for c in customers
      for w in warehouses
  ]))

  for c in customers:
    model.Add(sum([ship[c, w] for w in warehouses]) == 1)

  model.Add(sum(open) == p)

  for c in customers:
    for w in warehouses:
      model.Add(ship[c, w] <= open[w])

  # objective
  model.Minimize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('z:', solver.Value(z))
    print('open:', [solver.Value(open[w]) for w in warehouses])
    for c in customers:
      for w in warehouses:
        print(solver.Value(ship[c, w]), end=' ')
      print()
    print()

  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())

if __name__ == '__main__':
  main()
