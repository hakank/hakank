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

  Set covering in OR-tools CP-SAT Solver.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  '''
  Input Description: A set of subsets S_1, ..., S_m of the
  universal set U = {1,...,n}.

  Problem: What is the smallest subset of subsets T subset S such
  that \cup_{t_i in T} t_i = U?
  '''
  Data is from the pictures INPUT/OUTPUT.

  This is a port of my old CP model set_covering_skiena.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

def main():

  model = cp.CpModel()

  #
  # data
  #
  num_sets = 7
  num_elements = 12
  belongs = [
      # 1 2 3 4 5 6 7 8 9 0 1 2  elements
      [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],  # Set 1
      [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],  # 2
      [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0],  # 3
      [0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0],  # 4
      [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],  # 5
      [1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0],  # 6
      [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1]  # 7
  ]

  #
  # variables
  #
  x = [model.NewIntVar(0, 1, 'x[%i]' % i) for i in range(num_sets)]

  # number of choosen sets
  z = model.NewIntVar(0, num_sets * 2, 'z')

  # total number of elements in the choosen sets
  tot_elements = model.NewIntVar(0, num_sets * num_elements,"tot_elements")

  #
  # constraints
  #
  model.Add(z == sum(x))

  # all sets must be used
  for j in range(num_elements):
    model.Add(sum([belongs[i][j] * x[i] for i in range(num_sets)]) >= 1)

  # number of used elements
  model.Add(tot_elements == sum([
      x[i] * belongs[i][j] for i in range(num_sets) for j in range(num_elements)
  ]))

  # objective
  model.Minimize(z)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('z:', solver.Value(z))
    print('tot_elements:', solver.Value(tot_elements))
    print('x:', [solver.Value(x[i]) for i in range(num_sets)])

  print()
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
