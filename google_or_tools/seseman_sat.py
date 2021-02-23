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

  Seseman Convent problem in OR-tools CP-SAT Solver.

  n is the length of a border
  There are (n-2)^2 "holes", i.e.
  there are n^2 - (n-2)^2 variables to find out.

  The simplest problem, n = 3 (n x n matrix)
  which is represented by the following matrix:

   a b c
   d   e
   f g h

  Where the following constraints must hold:

    a + b + c = border_sum
    a + d + f = border_sum
    c + e + h = border_sum
    f + g + h = border_sum
    a + b + c + d + e + f = total_sum

  This is a port of my old CP model seseman.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n, x, total_sum):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__x = x
        self.__total_sum = total_sum
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1

        print("total_sum:", self.Value(self.__total_sum))
        for i in range(self.__n):
          for j in range(self.__n):
            print(self.Value(self.__x[(i, j)]), end=" ")
          print()
        print()


    def SolutionCount(self):
        return self.__solution_count


def main():

  model = cp.CpModel()

  # data
  n = 3
  border_sum = n * n

  # declare variables
  total_sum = model.NewIntVar(1, n * n * n * n, "total_sum")
  # x[0..n-1,0..n-1]
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = model.NewIntVar(0, n * n, "x %i %i" % (i, j))

  #
  # constraints
  #
  # zero all middle cells
  for i in range(1, n - 1):
    for j in range(1, n - 1):
      model.Add(x[(i, j)] == 0)

  # all borders must be >= 1
  for i in range(n):
    for j in range(n):
      if i == 0 or j == 0 or i == n - 1 or j == n - 1:
        model.Add(x[(i, j)] >= 1)

  # sum the borders (border_sum)
  model.Add(sum([x[(i, 0)] for i in range(n)]) == border_sum)
  model.Add(sum([x[(i, n - 1)] for i in range(n)]) == border_sum)
  model.Add(sum([x[(0, i)] for i in range(n)]) == border_sum)
  model.Add(sum([x[(n - 1, i)] for i in range(n)]) == border_sum)

  # total
  model.Add(
      sum([x[(i, j)] for i in range(n) for j in range(n)]) == total_sum)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(n, x,total_sum) 

  status = solver.SearchForAllSolutions(model,solution_printer)

  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution!")

  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())



if __name__ == "__main__":
  main()
