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

  Volsay problem in OR-tools CP-SAT Solver.

  From the OPL model volsay.mod
  Using arrays.

  This is a port of my old LP model volsay3.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

def main():

  model = cp.CpModel()
  
  # data
  num_products = 2

  products = ['Gas', 'Chloride']
  components = ['nitrogen', 'hydrogen', 'chlorine']

  demand = [[1, 3, 0], [1, 4, 1]]
  profit = [30, 40]
  stock = [50, 180, 40]

  # declare variables
  production = [
      model.NewIntVar(0, 100000, 'production[%i]' % i)
      for i in range(num_products)
  ]

  #
  # constraints
  #
  for c in range(len(components)):
    model.Add(
        sum([demand[p][c] * production[p]
                    for p in range(len(products))]) <= stock[c])

  # objective
  # Note: there is no support for solver.ScalProd in the LP/IP interface
  model.Maximize(
      sum([production[p] * profit[p] for p in range(num_products)]))

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)


  if status == cp.OPTIMAL:
    print('objective = ', solver.ObjectiveValue())
    for i in range(num_products):
      print(products[i], '=', solver.Value(production[i]), end=' ')
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()



if __name__ == '__main__':
  main()
