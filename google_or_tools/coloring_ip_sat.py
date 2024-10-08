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

  Simple coloring problem (MIP approach) in OR-tools CP-SAT Solver.

  Inspired by the GLPK:s model color.mod
  '''
  COLOR, Graph Coloring Problem

  Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>

  Given an undirected loopless graph G = (V, E), where V is a set of
  nodes, E <= V x V is a set of arcs, the Graph Coloring Problem is to
  find a mapping (coloring) F: V -> C, where C = {1, 2, ... } is a set
  of colors whose cardinality is as small as possible, such that
  F(i) != F(j) for every arc (i,j) in E, that is adjacent nodes must
  be assigned different colors.
  '''

  This is a port of my old OR-tools CP solver coloring_ip.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tols models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

def main():


  model = cp.CpModel()

  # max number of colors
  # [we know that 4 suffices for normal maps]
  nc = 5

  # number of nodes
  n = 11
  # set of nodes
  V = list(range(n))

  num_edges = 20

  #
  # Neighbours
  #
  # This data correspond to the instance myciel3.col from:
  # http://mat.gsia.cmu.edu/COLOR/instances.html
  #
  # Note: 1-based (adjusted below)
  E = [[1, 2], [1, 4], [1, 7], [1, 9], [2, 3], [2, 6], [2, 8], [3, 5], [3, 7],
       [3, 10], [4, 5], [4, 6], [4, 10], [5, 8], [5, 9], [6, 11], [7, 11],
       [8, 11], [9, 11], [10, 11]]

  #
  # declare variables
  #

  # x[i,c] = 1 means that node i is assigned color c
  x = {}
  for v in V:
    for j in range(nc):
      x[v, j] = model.NewIntVar(0, 1, 'v[%i,%i]' % (v, j))

  # u[c] = 1 means that color c is used, i.e. assigned to some node
  u = [model.NewIntVar(0, 1, 'u[%i]' % i) for i in range(nc)]

  # number of colors used, to minimize
  num_colors = model.NewIntVar(0,nc, "num_colors")
  model.Add(num_colors == sum(u))

  #
  # constraints
  #

  # each node must be assigned exactly one color
  for i in V:
    model.Add(sum([x[i, c] for c in range(nc)]) == 1)

  # adjacent nodes cannot be assigned the same color
  # (and adjust to 0-based)
  for i in range(num_edges):
    for c in range(nc):
      model.Add(x[E[i][0] - 1, c] + x[E[i][1] - 1, c] <= u[c])

  # objective
  model.Minimize(num_colors)

  #
  # solution
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print()
    print('number of colors:', solver.Value(num_colors))
    print('colors used:', [solver.Value(u[i]) for i in range(nc)])
    print()

    for v in V:
      print('v%i' % v, ' color ', end=' ')
      for c in range(nc):
        if solver.Value(x[v, c]) == 1:
          print(c)

  print()
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
