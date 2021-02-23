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

  Crypto problem in OR-tools CP-SAT Solver.

  Martin Gardner (February 1967):
  '''
  The integers 1,3,8, and 120 form a set with a remarkable property: the
  product of any two integers is one less than a perfect square. Find
  a fifth number that can be added to the set without destroying
  this property.
  '''

  Solution: The number is 0.

  There are however other sets of five numbers with this property.
  Here are the one in the range of 0.10000:
  [0, 1, 3, 8, 120]
  [0, 1, 3, 120, 1680]
  [0, 1, 8, 15, 528]
  [0, 1, 8, 120, 4095]
  [0, 1, 15, 24, 1520]
  [0, 1, 24, 35, 3480]
  [0, 1, 35, 48, 6888]
  [0, 2, 4, 12, 420]
  [0, 2, 12, 24, 2380]
  [0, 2, 24, 40, 7812]
  [0, 3, 5, 16, 1008]
  [0, 3, 8, 21, 2080]
  [0, 3, 16, 33, 6440]
  [0, 4, 6, 20, 1980]
  [0, 4, 12, 30, 5852]
  [0, 5, 7, 24, 3432]
  [0, 6, 8, 28, 5460]
  [0, 7, 9, 32, 8160]

  This is a port of my old CP model curious_set_of_integers.py


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import *


def main(show_all=0):

  model = cp.CpModel()

  #
  # data
  #
  n = 5
  max_val = 10_000

  #
  # variables
  #
  x = [model.NewIntVar(0, max_val, "x[%i]" % i) for i in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(x)
  increasing(model, x)

  for i in range(n):
    for j in range(i):
      p = model.NewIntVar(0, max_val, "p[%i,%i]" % (i, j))
      pp = model.NewIntVar(0, max_val*max_val, "pp[%i,%i]" % (i, j))
      model.AddMultiplicationEquality(pp,[p,p])
      xx = model.NewIntVar(0, max_val*max_val, "xx[%i,%i]" % (i, j))
      model.AddMultiplicationEquality(xx,[x[i],x[j]])
      model.Add(pp - 1 == xx)

  # This is the original problem:
  # Which is the fifth number?
  v = [1, 3, 8, 120]
  if show_all == 0:
    b = [model.NewBoolVar("") for i in range(n)]
    for i in range(n):
      count_vars(model,v,x[i],b[i])
    model.Add(sum(b) == 4)

  # model.AddDecisionStrategy(x, 
  #                           cp.CHOOSE_MIN_DOMAIN_SIZE, # cp.CHOOSE_FIRST,
  #                           cp.SELECT_LOWER_HALF # cp.SELECT_MIN_VALUE
  #                           )

  #
  # search and result
  #
  solver = cp.CpSolver()
  solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.search_branching = cp.FIXED_SEARCH
  solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  solution_printer = ListPrinter(x)
  # status = solver.Solve(model)
  status = solver.SearchForAllSolutions(model,solution_printer)

  if status == cp.OPTIMAL:
    xval = [solver.Value(x[i]) for i in range(n)]
    if show_all == 0:
      print("x:", xval)
      for xv in xval:
        if not xv in v:
          print("missing number is", xv)

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  print("Original problem:")
  main(0)
  ## This finds all the solutions (max_val = 10000)
  ## quite fast, but then it takes a long time to 
  ## finish the search.
  # print("\nFind all numbers with this property:")
  # main(1)
