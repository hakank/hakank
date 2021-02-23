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

  Assignment problem in Google CP Solver.

  Winston 'Operations Research', Assignment Problems, page 393f
  (generalized version with added test column)

  This is a port of my old CP model assignment.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google OR-tools models:
  http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, rows,cols, x, total_cost):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__rows = rows
        self.__cols = cols
        self.__x = x
        self.__total_cost = total_cost
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("Total cost: ", self.Value(self.__total_cost))
        for i in range(rows):
          for j in range(cols):
            print(self.Value(self.__x[i][j]),end=" ")
          print()
        print()

    def SolutionCount(self):
        return self.__solution_count




def main(cost, rows, cols):

  model = cp.CpModel()

  #
  # data
  #

  # declare variables
  total_cost = model.NewIntVar(0, 100, "total_cost")
  x = []
  for i in range(rows):
    t = []
    for j in range(cols):
      t.append(model.NewIntVar(0, 1, "x[%i,%i]" % (i, j)))
    x.append(t)
  x_flat = [x[i][j] for i in range(rows) for j in range(cols)]

  #
  # constraints
  #

  # total_cost
  scalprod = [cp.LinearExpr.ScalProd(x_row, cost_row) for (x_row, cost_row) in zip(x, cost)]
  model.Add(total_cost == sum(scalprod))
  # model.Add(total_cost == sum(
  #    [model.LinearExpr.ScalProd(x_row, cost_row) for (x_row, cost_row) in zip(x, cost)])
  #    )

  # exacly one assignment per row, all rows must be assigned
  [
      model.Add(sum([x[row][j]
                             for j in range(cols)]) == 1)
      for row in range(rows)
  ]

  # zero or one assignments per column
  [
      model.Add(sum([x[i][col]
                             for i in range(rows)]) <= 1)
      for col in range(cols)
  ]
  model.Minimize(total_cost)

  #
  # Search and print solution
  #
  solver = cp.CpSolver() 
  
  # We only print the intermediate values of total_cost, not x
  solution_printer = SolutionPrinter(rows,cols, x,total_cost)
  status = solver.SolveWithSolutionCallback(model,solution_printer)
  # status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("\ntotal_cost:", solver.Value(total_cost))
    for i in range(rows):
      for j in range(cols):
        print(solver.Value(x[i][j]), end=" ")
      print()
    print()

    for i in range(rows):
      print("Task:", i, end=" ")
      for j in range(cols):
        if solver.Value(x[i][j]) == 1:
          print(" is done by ", j)
    print()

  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


# Problem instance
# hakank: I added the fifth column to make it more
#         interesting
rows = 4
cols = 5
cost = [[14,  5,  8,  7, 15], 
        [ 2, 12,  6,  5,  3], 
        [ 7,  8,  3,  9,  7], 
        [ 2,  4,  6, 10,  1]]

if __name__ == "__main__":
  main(cost, rows, cols)
