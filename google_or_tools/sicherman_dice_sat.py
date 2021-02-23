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

  Sicherman Dice in OR-tools CP-SAT Solver.

  From http://en.wikipedia.org/wiki/Sicherman_dice
  ""
  Sicherman dice are the only pair of 6-sided dice which are not normal dice,
  bear only positive integers, and have the same probability distribution for
  the sum as normal dice.

  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  ""

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:

    x1 = [1, 2, 3, 4, 5, 6]
    x2 = [1, 2, 3, 4, 5, 6]

    x1 = [1, 2, 2, 3, 3, 4]
    x2 = [1, 3, 4, 5, 6, 8]


  Extra: If we also allow 0 (zero) as a valid value then the
  following two solutions are also valid:

    x1 = [0, 1, 1, 2, 2, 3]
    x2 = [2, 4, 5, 6, 7, 9]

    x1 = [0, 1, 2, 3, 4, 5]
    x2 = [2, 3, 4, 5, 6, 7]

  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html


  This is a port of my old CP model sicherman_dice.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x1, x2):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x1 = x1
        self.__x2 = x2
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("x1:", [self.Value(v) for v in self.__x1])
        print("x1:", [self.Value(v) for v in self.__x2])
        print()

    def SolutionCount(self):
        return self.__solution_count


def main(min_val=1):

  model = cp.CpModel()

  #
  # data
  #
  n = 6
  m = 10

  # standard distribution
  standard_dist = [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1]

  #
  # declare variables
  #

  # the two dice
  x1 = [model.NewIntVar(min_val, m, "x1(%i)" % i) for i in range(n)]
  x2 = [model.NewIntVar(min_val, m, "x2(%i)" % i) for i in range(n)]

  #
  # constraints
  #
  # [solver.Add(standard_dist[k] == solver.Sum([x1[i] + x2[j] == k+2 for i in range(n) for j in range(n)]))
  # for k in range(len(standard_dist))]
  for k in range(len(standard_dist)):
    tmp = [model.NewBoolVar("") for i in range(n) for j in range(n)]
    for i in range(n):
      for j in range(n):        
        # model.Add(tmp[i * n + j] == solver.IsEqualCstVar(x1[i] + x2[j], k + 2)) # old
        model.Add(x1[i] + x2[j] == k + 2).OnlyEnforceIf(tmp[i*n+j])
        model.Add(x1[i] + x2[j] != k + 2).OnlyEnforceIf(tmp[i*n+j].Not())

    model.Add(standard_dist[k] == sum(tmp))

  # symmetry breaking
  [model.Add(x1[i] <= x1[i + 1]) for i in range(n - 1)],
  [model.Add(x2[i] <= x2[i + 1]) for i in range(n - 1)],
  [model.Add(x1[i] <= x2[i]) for i in range(n - 1)],

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(x1,x2)
  status = solver.SearchForAllSolutions(model,solution_printer)

  if status != cp.OPTIMAL:
    print("No solution!")


  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  print("min_val=1")
  main(1)
  print("\nmin_val=0")
  main(0)
