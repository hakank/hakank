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

  Problem from
  Katta G. Murty: 'Optimization Models for Decision Making', page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf

  10 senators making a committee, where there must at least be one
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10

  The objective is to minimize the number of senators.

  This is a port of my old CP model set_covering3.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/

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
  num_groups = 6
  num_senators = 10

  # which group does a senator belong to?
  belongs = [
      [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],  # 1 southern
      [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],  # 2 northern
      [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],  # 3 liberals
      [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],  # 4 conservative
      [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],  # 5 democrats
      [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]  # 6 republicans
  ]

  #
  # declare variables
  #
  x = [model.NewIntVar(0, 1, "x[%i]" % i) for i in range(num_senators)]
  z = model.NewIntVar(0,num_senators,"z")

  #
  # constraints
  #

  # number of assigned senators (to minimize)
  model.Add(z == sum(x))

  # ensure that each group is covered by at least
  # one senator
  for i in range(num_groups):
    model.Add(
        sum([x[j] * belongs[i][j] for j in range(num_senators)]) >= 1)

  model.Minimize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("z:", solver.Value(z))
    print("x:", [solver.Value(x[i]) for i in range(num_senators)])
    for j in range(num_senators):
      if solver.Value(x[j]) == 1:
        print("Senator", j + 1, "belongs to these groups:", end=" ")
        for i in range(num_groups):
          if belongs[i][j] == 1:
            print(i + 1, end=" ")
        print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
