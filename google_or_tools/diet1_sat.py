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

  Simple diet problem in OR-tools CP-SAT Solver.


  Minimize the cost for the products:
  Type of                        Calories   Chocolate    Sugar    Fat
  Food                                      (ounces)     (ounces) (ounces)
  Chocolate Cake (1 slice)       400           3            2      2
  Chocolate ice cream (1 scoop)  200           2            2      4
  Cola (1 bottle)                150           0            4      1
  Pineapple cheesecake (1 piece) 500           0            4      5

  This is a port of my old CP model diet1.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import scalar_product


def main(unused_argv):
  
  model = cp.CpModel()

  #
  # data
  #
  n = 4
  price = [50, 20, 30, 80]  # in cents
  limits = [500, 6, 10, 8]  # requirements for each nutrition type

  # nutritions for each product
  calories  = [400, 200, 150, 500]
  chocolate = [3, 2, 0, 0]
  sugar     = [2, 2, 4, 4]
  fat       = [2, 4, 1, 5]

  #
  # declare variables
  #
  x = [model.NewIntVar(0, 100, "x%d" % i) for i in range(n)]
  cost = model.NewIntVar(0, 10000, "cost")
  

  #
  # constraints
  #
  model.Add(sum(x[i] * calories[i] for i in range(n)) >= limits[0])
  model.Add(sum(x[i] * chocolate[i] for i in range(n)) >= limits[1])
  model.Add(sum(x[i] * sugar[i] for i in range(n)) >= limits[2])
  model.Add(sum(x[i] * fat[i] for i in range(n)) >= limits[3])

  scalar_product(model, price,x, cost)

  # objective
  model.Minimize(cost)

  # Solve model.
  solver = cp.CpSolver()
  status = solver.Solve(model)

  # Output solution.
  if status == cp.OPTIMAL:
    print("cost:", solver.ObjectiveValue())
    print([("abcdefghij" [i], solver.Value(x[i])) for i in range(n)])
    print()
    print("Status      :", solver.StatusName(status))
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches :", solver.NumBranches())
    print("WallTime    :", solver.WallTime())
    print()


if __name__ == "__main__":
  main("cp sample")
