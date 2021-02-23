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

  Simple coloring problem using MIP in OR-tools CP-SAT Solver.

  Problem instance from GLPK:s model color.mod

  This is a port of my old OR-tools CP model.

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""

import sys
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main():

  model = cp.CpModel()

  # max number of colors
  # [we know that 4 suffices for normal, planar, maps]
  nc = 5

  # number of nodes
  n = 11
  # set of nodes
  V = range(n)

  num_edges = 20

  #
  # Neighbours
  #
  # This data correspond to the instance myciel3.col from:
  # http://mat.gsia.cmu.edu/COLOR/instances.html
  #
  # Note: 1-based (adjusted below)
  E =  [[1, 2],
        [1, 4],
        [1, 7],
        [1, 9],
        [2, 3],
        [2, 6],
        [2, 8],
        [3, 5],
        [3, 7],
        [3, 10],
        [4, 5],
        [4, 6],
        [4, 10],
        [5, 8],
        [5, 9],
        [6, 11],
        [7, 11],
        [8, 11],
        [9, 11],
        [10, 11]]


  #
  # decision variables
  #
  x = [model.NewIntVar(1, nc, 'x[%i]' % i) for i in V]
  max_color = model.NewIntVar(0,nc,"max_color")

  # number of colors used, to minimize
  # max_color = model.Max(x).Var()
  model.AddMaxEquality(max_color,x)

  #
  # constraints
  #

  # adjacent nodes cannot be assigned the same color
  # (and adjust to 0-based)
  for i in range(num_edges):
     model.Add(x[E[i][0]-1] != x[E[i][1]-1])

  # symmetry breaking
  # solver.Add(x[0] == 1);
  # solver.Add(x[1] <= 2);
  for i in range(nc):
      model.Add(x[i] <= i+1);

  # objective (minimize the number of colors)
  model.Minimize(max_color)

  #
  # solution
  #
  solver = cp.CpSolver() 
  status = solver.Solve(model)
  
  if status == cp.OPTIMAL:
    print("x:", [solver.Value(x[i]) for i in V])
    print("max_color:", solver.Value(max_color))
    print()

  print()
  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == '__main__':
    main()
