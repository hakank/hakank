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

  Knapsack problem in OR-tools CP-SAT Solver.

  Simple knapsack problem.

  This is a port of my old CP model knapsack_cp.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import knapsack


def main(values, weights, n):

  model = cp.CpModel()

  #
  # data
  #
  print("values:", values)
  print("weights:", weights)
  print("n:", n)
  print()

  # declare variables

  #
  # constraints
  #
  x, z, w = knapsack(model, values, weights, n)

  # objective
  model.Maximize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("x:", [solver.Value(x[i]) for i in range(len(values))])
    print("z:", solver.Value(z))
    print("w:", solver.Value(w))
    print()

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


values = [15, 100, 90, 60, 40, 15, 10, 1, 12, 12, 100]
weights = [2, 20, 20, 30, 40, 30, 60, 10, 21, 12, 2]
n = 102

if __name__ == "__main__":
  main(values, weights, n)
