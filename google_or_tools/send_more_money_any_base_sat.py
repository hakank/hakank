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

  SEND+MORE=MONEY in 'any' base in OR-tools CP-SAT Solver.

  Alphametic problem SEND+MORE=MONEY in any base.

  Examples:
  Base 10 has one solution:
     {9, 5, 6, 7, 1, 0, 8, 2}
  Base 11 has three solutions:
     {10, 5, 6, 8, 1, 0, 9, 2}
     {10, 6, 7, 8, 1, 0, 9, 3}
     {10, 7, 8, 6, 1, 0, 9, 2}

  This is a port of my of CP model send_more_money_any_base.py


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter


def main(base=10):

  model = cp.CpModel()
  
  # data
  print('base:', base)

  # declare variables
  s = model.NewIntVar(0, base - 1, 's')
  e = model.NewIntVar(0, base - 1, 'e')
  n = model.NewIntVar(0, base - 1, 'n')
  d = model.NewIntVar(0, base - 1, 'd')
  m = model.NewIntVar(0, base - 1, 'm')
  o = model.NewIntVar(0, base - 1, 'o')
  r = model.NewIntVar(0, base - 1, 'r')
  y = model.NewIntVar(0, base - 1, 'y')

  x = [s, e, n, d, m, o, r, y]

  #
  # constraints
  #
  model.AddAllDifferent(x)
  model.Add(
      s * base**3 + e * base**2 + n * base + d + m * base**3 + o * base**2 +
      r * base + e == m * base**4 + o * base**3 + n * base**2 + e * base + y,)
  model.Add(s > 0)
  model.Add(m > 0)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = ListPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if status != cp.OPTIMAL:
    print("No solution!")

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())
  print()


base = 10
if __name__ == '__main__':
  # for base in range(10,30):
  #    main(base)
  if len(sys.argv) > 1:
    base = int(sys.argv[1])

  main(base)
