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

  Magic squares in Google CP-SAT Solver.

  Magic square problem.
  See https://en.wikipedia.org/wiki/Magic_square
  '''
  In recreational mathematics, a square array of numbers, usually positive integers, 
  is called a magic square if the sums of the numbers in each row, each column, 
  and both main diagonals are the same. The order of the magic square is the number of 
  integers along one side (n), and the constant sum is called the magic constant. 
  If the array includes just the positive integers 1,2,...,n^2, the magic square is 
  said to be normal. Some authors take magic square to mean normal magic squares.
  '''

  This is a port of my old CP model magic_square.py

  Times to first solution for n=1..12 (using benchmark())

  n   WallTime (s)
  -----------------
  1     0.000127245
  2     5.2747e-05
  3     0.007739887000000001
  4     0.01025156
  5     0.033206056000000005
  6     0.22720014900000002
  7     0.45328528900000004
  8     1.1386696120000002
  9     1.573457436
  10    3.6889745940000003
  11    1.6905479950000002
  12    55.631923315

  Here's one solution of n=12 (it took 55.6s)
    s: 870
    107  51  74 124  86  41  55  54  89  68   9 112 
     93  81  75 123  43  85  71 118  84  77   5  15 
     29  30  32  21 110  79 111 119  31  34 143 131 
     26 127  25  60 126  17 125  28  33  27 144 132 
     88 108  69  22  91  62  73  65  57  46 142  47 
     42  38 102  19  45 128  97  23  44  61 141 130 
     78 113  66  18  76  95  16  70  82 109 136  11 
      7   6 139   8  12 134  20 133 137 138   1 135 
     94 106  39 117  64  83 104  72  80  96   2  13 
     92  59  99 122  40  58  56  24  67 129   4 120 
    116 103 100 121  90  35  37 101 114  36   3  14 
     98  48  50 115  87  53 105  63  52  49 140  10 



  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n, s, x, limit=0):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n 
        self.__s = s
        self.__x = x
        self.__limit = limit
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print(f"Solution #{self.__solution_count}")
        print("s:", self.Value(self.__s))
        for i in range(self.__n):
          for j in range(self.__n):
            print("%3i" % self.Value(self.__x[(i, j)]), end=" ")
          print()
        print()

        if self.__limit > 0 and self.__solution_count >= self.__limit:
          self.StopSearch() 


    def SolutionCount(self):
        return self.__solution_count



def main(n=4, limit=0):

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
      x[(i, j)] = model.NewIntVar(1, n * n, "x(%i,%i)" % (i, j))
  x_flat = [x[(i, j)] for i in range(n) for j in range(n)]

  # the sum
  # s = ( n * (n*n + 1)) / 2
  s = model.NewIntVar(1, n * n * n, "s")

  #
  # constraints
  #
  nn = ( n * (n*n + 1)) // 2
  model.Add(s == nn)

  model.AddAllDifferent(x_flat)

  [model.Add(sum([x[(i, j)] for j in range(n)]) == s) for i in range(n)]
  [model.Add(sum([x[(i, j)] for i in range(n)]) == s) for j in range(n)]

  model.Add(sum([x[(i, i)] for i in range(n)]) == s)  # diag 1
  model.Add(sum([x[(i, n - i - 1)] for i in range(n)]) == s)  # diag 2

  # symmetry breaking
  # model.Add(x[(0,0)] == 1)

  #
  # solution and search
  #
  solver = cp.CpSolver()

  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  # solver.parameters.linearization_level = 0
  # solver.parameters.cp_model_probing_level = 0

  # status = solver.Solve(model)
  solution_printer = SolutionPrinter(n, s, x, limit)
  # solution_printer = SimpleSolutionCounter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not (status == cp.FEASIBLE or status == cp.OPTIMAL):
    print("No solution found!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


def benchmark():
  for n in range(1,13):
    main(n, 1)

n = 3
limit=0
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  if len(sys.argv) > 2:
    limit = int(sys.argv[2])

  main(n, limit)

  # benchmark()
