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

  Max flow problem (Taha) in OR-tools CP-SAT Solver.

  From Taha 'Introduction to Operations Research', Example 6.4-2

  Translated from the AMPL code at
  http://taha.ineg.uark.edu/maxflo.txt

  This is a port of my old CP model max_flow_taha.py

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
  start = 0
  end = n - 1

  nodes = list(range(n))

  # cost matrix
  c = [[0, 20, 30, 10, 0], [0, 0, 40, 0, 30], [0, 0, 0, 10, 20],
       [0, 0, 5, 0, 20], [0, 0, 0, 0, 0]]

  #
  # declare variables
  #
  x = {}
  for i in nodes:
    for j in nodes:
      x[i, j] = model.NewIntVar(0, c[i][j], 'x[%i,%i]' % (i, j))

  x_flat = [x[i, j] for i in nodes for j in nodes]
  out_flow = [model.NewIntVar(0, 10000, 'out_flow[%i]' % i) for i in nodes]
  in_flow = [model.NewIntVar(0, 10000, 'in_flow[%i]' % i) for i in nodes]

  total = model.NewIntVar(0, 10000, 'z')

  #
  # constraints
  #
  model.Add(total == sum([x[start, j] for j in nodes if c[start][j] > 0]))

  for i in nodes:
    model.Add(in_flow[i] == sum([x[j, i] for j in nodes if c[j][i] > 0]))
    model.Add(out_flow[i] == sum([x[i, j] for j in nodes if c[i][j] > 0]))

  # in_flow == out_flow
  for i in nodes:
    if i != start and i != end:
      model.Add(out_flow[i] - in_flow[i] == 0)

  s1 = [x[i, start] for i in nodes if c[i][start] > 0]
  if len(s1) > 0:
    model.Add(sum([x[i, start] for i in nodes if c[i][start] > 0] == 0))

  s2 = [x[end, j] for j in nodes if c[end][j] > 0]
  if len(s2) > 0:
    model.Add(sum([x[end, j] for j in nodes if c[end][j] > 0]) == 0)

  # objective: maximize total cost
  model.Maximize(total)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('total:', solver.Value(total))
    print('in_flow:', [solver.Value(in_flow[i]) for i in nodes])
    print('out_flow:', [solver.Value(out_flow[i]) for i in nodes])
    for i in nodes:
      for j in nodes:
        print('%2i' % solver.Value(x[i, j]), end=' ')
      print()
    print()

  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
