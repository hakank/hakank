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

  SEND+MOST=MONEY in OR-tools CP-SAT Solver.

  Alphametic problem were we maximize MONEY.

  Problem from the lecture notes:
  http://www.ict.kth.se/courses/ID2204/notes/L01.pdf

  This is a port of my old CP model send_most_money.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter


def main(MONEY=0):

  model = cp.CpModel()

  # data

  # declare variables
  s = model.NewIntVar(0, 9, 's')
  e = model.NewIntVar(0, 9, 'e')
  n = model.NewIntVar(0, 9, 'n')
  d = model.NewIntVar(0, 9, 'd')
  m = model.NewIntVar(0, 9, 'm')
  o = model.NewIntVar(0, 9, 'o')
  t = model.NewIntVar(0, 9, 't')
  y = model.NewIntVar(0, 9, 'y')
  money = model.NewIntVar(0, 100000, 'money')

  x = [s, e, n, d, m, o, t, y]

  #
  # constraints
  #
  if MONEY > 0:
    model.Add(money == MONEY)

  model.AddAllDifferent(x)
  model.Add(money == m * 10000 + o * 1000 + n * 100 + e * 10 + y)
  model.Add(money > 0)
  model.Add(1000 * s + 100 * e + 10 * n + d + 1000 * m + 100 * o + 10 * s +
             t == money)
  model.Add(s > 0)
  model.Add(m > 0)

  if MONEY == 0:
    model.Maximize(money)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  
  if MONEY == 0:
    status = solver.Solve(model)
  else:
    solution_printer = ListPrinter(x)
    status = solver.SearchForAllSolutions(model, solution_printer)

  money_val = 0
  if MONEY == 0 and status == cp.OPTIMAL:
    money_val = solver.Value(money)
    print("Optimal value of money:", money_val)
  else:
    print()
    print('NumSolutions:', solution_printer.SolutionCount())

  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())

  if MONEY == 0:
    return money_val


if __name__ == '__main__':
  # First get the maximised MONEY, and then show all solutions for
  # this value
  print('Maximize "money"...')
  money = main(0)
  print('\nCheck all solutions for "money"=%i' % money)
  main(money)
