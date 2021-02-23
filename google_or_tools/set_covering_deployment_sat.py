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

  Set covering deployment in OR-tools CP-SAT Solver

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  '''
  Set covering deployment (sometimes written 'set-covering deployment'
  and abbreviated SCDP for 'set covering deployment problem') seeks
  an optimal stationing of troops in a set of regions so that a
  relatively small number of troop units can control a large
  geographic region. ReVelle and Rosing (2000) first described
  this in a study of Emperor Constantine the Great's mobile field
  army placements to secure the Roman Empire.
  '''

  This is a port of my of CP model set_covering_deployment.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
# import math, sys
# from cp_sat_utils import *



def main():

  model = cp.CpModel()

  #
  # data
  #

  countries = [
      "Alexandria", "Asia Minor", "Britain", "Byzantium", "Gaul", "Iberia",
      "Rome", "Tunis"
  ]
  n = len(countries)

  # the incidence matrix (neighbours)
  mat = [[0, 1, 0, 1, 0, 0, 1, 1], 
         [1, 0, 0, 1, 0, 0, 0, 0],
         [0, 0, 0, 0, 1, 1, 0, 0], 
         [1, 1, 0, 0, 0, 0, 1, 0],
         [0, 0, 1, 0, 0, 1, 1, 0], 
         [0, 0, 1, 0, 1, 0, 1, 1],
         [1, 0, 0, 1, 1, 1, 0, 1], 
         [1, 0, 0, 0, 0, 1, 1, 0]]

  #
  # declare variables
  #

  # First army
  X = [model.NewIntVar(0, 1, "X[%i]" % i) for i in range(n)]

  # Second (reserv) army
  Y = [model.NewIntVar(0, 1, "Y[%i]" % i) for i in range(n)]

  num_armies = model.NewIntVar(0, n, "num_armies")

  #
  # constraints
  #

  # total number of armies
  model.Add(num_armies == sum([X[i] + Y[i] for i in range(n)]))

  #
  #  Constraint 1: There is always an army in a city
  #                (+ maybe a backup)
  #                Or rather: Is there a backup, there
  #                must be an an army
  #
  [model.Add(X[i] >= Y[i]) for i in range(n)]

  #
  # Constraint 2: There should always be an backup army near every city
  #
  for i in range(n):
    model.Add(X[i] + sum([Y[j] for j in range(n) if mat[i][j] == 1]) >= 1)

  model.Minimize(num_armies)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("num_armies:", solver.Value(num_armies))
    print("X:", [solver.Value(X[i]) for i in range(n)])
    print("Y:", [solver.Value(Y[i]) for i in range(n)])

    for i in range(n):
      if solver.Value(X[i]) == 1:
        print("army:", countries[i], end=" ")
      if solver.Value(Y[i]) == 1:
        print("reserv army:", countries[i], " ")
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
