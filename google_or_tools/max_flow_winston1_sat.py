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

  Max flow problem in OR-tools CP Solver.

  From Winston 'Operations Research', page 420f, 423f
  Sunco Oil example.

  This is a port of my old CP model max_flow_winston1.py

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
  n = 5
  nodes = list(range(n))

  # the arcs
  # Note:
  # This is 1-based to be compatible with other
  # implementations.
  arcs1 = [[1, 2], [1, 3], [2, 3], [2, 4], [3, 5], [4, 5], [5, 1]]

  # convert arcs to 0-based
  arcs = []
  for (a_from, a_to) in arcs1:
    a_from -= 1
    a_to -= 1
    arcs.append([a_from, a_to])

  num_arcs = len(arcs)

  # capacities
  cap = [2, 3, 3, 4, 2, 1, 100]

  # convert arcs to matrix
  # for sanity checking below
  mat = {}
  for i in nodes:
    for j in nodes:
      c = 0
      for k in range(num_arcs):
        if arcs[k][0] == i and arcs[k][1] == j:
          c = 1
      mat[i, j] = c

  #
  # declare variables
  #
  flow = {}
  for i in nodes:
    for j in nodes:
      flow[i, j] = model.NewIntVar(0, 200, 'flow %i %i' % (i, j))

  flow_flat = [flow[i, j] for i in nodes for j in nodes]

  z = model.NewIntVar(0, 10000, 'z')

  #
  # constraints
  #
  model.Add(z == flow[n - 1, 0])

  # capacity of arcs
  for i in range(num_arcs):
    model.Add(flow[arcs[i][0], arcs[i][1]] <= cap[i])

  # inflows == outflows
  for i in nodes:
    model.Add(sum([flow[arcs[k][0], arcs[k][1]] 
                      for k in range(num_arcs) if arcs[k][1] == i
                    ])
        == 
        sum([flow[arcs[k][0], arcs[k][1]] 
                      for k in range(num_arcs) if arcs[k][0] == i
                    ]))

  # sanity: just arcs with connections can have a flow
  for i in nodes:
    for j in nodes:
      if mat[i, j] == 0:
        model.Add(flow[i, j] == 0)

  # objective: maximize z
  model.Maximize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('z:', solver.Value(z))
    for i in nodes:
      for j in nodes:
        print(solver.Value(flow[i, j]), end=' ')
      print()
    print()

  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
