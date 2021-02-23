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

  Subset sum problem in OR-tools CP-SAT Solver.

  From Katta G. Murty: 'Optimization Models for Decision Making', page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  '''
  Example 7.8.1

  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  '''

  This is a port of my old CP model subset_sum.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-ools models:
  http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def subset_sum(model, values, total):
  n = len(values)
  x = [model.NewIntVar(0, n,f"x[{i}") for i in range(n)]
  ss = model.NewIntVar(0, n,"ss")

  model.Add(ss == sum(x))
  model.Add(total == cp.LinearExpr.ScalProd(x, values))
  return x, ss


def main(coins, total):

  model = cp.CpModel()

  #
  # data
  #
  print("coins:", coins)
  print("total:", total)
  print()

  #
  # declare variables
  #

  #
  # constraints
  #
  x, ss = subset_sum(model, coins, total)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)
  print("status:", solver.StatusName(status))


  if status == cp.OPTIMAL:
    print("ss:", solver.Value(ss))
    print("x: ", [solver.Value(x[i]) for i in range(len(x))])
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


coins = [16, 17, 23, 24, 39, 40]
total = 100
if __name__ == "__main__":
  if len(sys.argv) > 1:
    total = int(sys.argv[1])
  main(coins, total)
