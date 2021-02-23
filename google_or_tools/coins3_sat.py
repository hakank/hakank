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

  Coin application in Google CP Solver.

  From 'Constraint Logic Programming using ECLiPSe'
  pages 99f and 234 ff.
  The solution in ECLiPSe is at page 236.

  '''
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  '''

  This is a port of my old OR-tools CP model coins3.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import scalar_product, SimpleSolutionPrinter



def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 6  # number of different coins
  variables = [1, 2, 5, 10, 25, 50]

  # declare variables
  x = [model.NewIntVar(0, 99, "x%i" % i) for i in range(n)]
  num_coins = model.NewIntVar(0, 99, "num_coins")

  #
  # constraints
  #

  # number of used coins, to be minimized
  model.Add(num_coins == sum(x))

  # Check that all changes from 1 to 99 can be made.
  for j in range(1, 100):
    tmp = [model.NewIntVar(0, 99, "b%i" % i) for i in range(n)]
    model.Add(j == cp.LinearExpr.ScalProd(tmp,variables))
    # scalar_product(model,tmp,variables,j)
    [model.Add(tmp[i] <= x[i]) for i in range(n)]

  # objective
  # objective = solver.Minimize(num_coins, 1)
  model.Minimize(num_coins)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  # status = solver.Solve(model)
  solution_printer = SimpleSolutionPrinter([num_coins])
  status = solver.SolveWithSolutionCallback(model, solution_printer)

  if status == cp.OPTIMAL:
    print("x: ", [solver.Value(x[i]) for i in range(n)])
    print("num_coins:", solver.Value(num_coins))

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
