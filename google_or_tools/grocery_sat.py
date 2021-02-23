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

  Grocery problem in OR-tools CP-SAT Solver.

  From  Christian Schulte, Gert Smolka, Finite Domain
  http://www.mozart-oz.org/documentation/fdt/
  Constraint Programming in Oz. A Tutorial. 2001.
  '''
  A kid goes into a grocery store and buys four items. The cashier
  charges $7.11, the kid pays and is about to leave when the cashier
  calls the kid back, and says 'Hold on, I multiplied the four items
  instead of adding them; I'll try again; Hah, with adding them the
  price still comes to $7.11'. What were the prices of the four items?
  '''

  This is a port of my old CP model grocery.py
  
  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import prod, increasing
from functools import reduce




def main():

  model = cp.CpModel()
  #
  # data
  #
  n = 4
  c = 711

  #
  # declare variables
  #
  item = [model.NewIntVar(0, c, "item[%i]" % i) for i in range(n)]

  #
  # constraints
  #
  model.Add(sum(item) == c)
  prod(model, item, c * 100**3)

  # symmetry breaking
  increasing(model, item)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)
  if status == cp.OPTIMAL:
    print("item:", [solver.Value(item[i]) for i in range(n)])
    print()

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
