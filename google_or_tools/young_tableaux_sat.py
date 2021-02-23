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

  Young tableaux in OR-tools CP-SAT Solver.

  See
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  '''
  The partitions of 4 are
  {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}

  And the corresponding standard Young tableaux are:

    1.   1 2 3 4

    2.   1 2 3         1 2 4    1 3 4
            4             3        2

    3.   1 2           1 3
        3 4           2 4

    4    1 2           1 3      1 4
        3             2        2
        4             4        3

    5.   1
        2
        3
        4
      '''

  This is a port of my old CP model young_tableaux.py  

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import global_cardinality


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n, x_flat, p):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__x_flat = x_flat
        self.__p = p
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        n = self.__n
        print("p:", [self.Value(self.__p[i]) for i in range(n)])
        print("x:")
        for i in range(n):
          for j in range(n):
            val = self.Value(self.__x_flat[i * n + j])
            if val <= n:
              print(val, end=" ")
          if self.Value(self.__p[i]) > 0:
            print()
        print()


    def SolutionCount(self):
        return self.__solution_count


def main(n=5):

  model = cp.CpModel()

  #
  # data
  #
  print("n:", n)

  #
  # declare variables
  #
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = model.NewIntVar(1, n + 1, "x(%i,%i)" % (i, j))

  x_flat = [x[(i, j)] for i in range(n) for j in range(n)]

  # partition structure
  p = [model.NewIntVar(0, n + 1, "p%i" % i) for i in range(n)]

  #
  # constraints
  #

  # 1..n is used exactly once
  global_cardinality(model,x_flat,[i for i in range(1, n+1)], [1 for _ in range(n)])

  model.Add(x[(0, 0)] == 1)

  # row wise
  for i in range(n):
    for j in range(1, n):
      model.Add(x[(i, j)] >= x[(i, j - 1)])

  # column wise
  for j in range(n):
    for i in range(1, n):
      model.Add(x[(i, j)] >= x[(i - 1, j)])

  # calculate the structure (the partition)
  for i in range(n):
    # MiniZinc/Zinc version:
    # p[i] == sum(j in 1..n) (bool2int(x[i,j] <= n))
    b = [model.NewBoolVar("") for j in range(n)]
    for j in range(n):
      model.Add(x[i,j] <= n).OnlyEnforceIf(b[j])

    model.Add(p[i] == sum(b))

  model.Add(sum(p) == n)

  for i in range(1, n):
    model.Add(p[i - 1] >= p[i])

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(n, x_flat, p)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution!")


  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


n = 5
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])

  main(n)
