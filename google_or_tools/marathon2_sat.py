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

  Marathon puzzle in OR-tools CP-SAT Solver.

  From Xpress example
  http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
  '''
  Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
  have arrived as the first six at the Paris marathon.
  Reconstruct their arrival order from the following
  information:
  a) Olivier has not arrived last
  b) Dominique, Pascal and Ignace have arrived before Naren
     and Olivier
  c) Dominique who was third last year has improved this year.
  d) Philippe is among the first four.
  e) Ignace has arrived neither in second nor third position.
  f) Pascal has beaten Naren by three positions.
  g) Neither Ignace nor Dominique are on the fourth position.

     (c) 2002 Dash Associates
    author: S. Heipcke, Mar. 2002
  '''

  This is a port of my old CP model marathon2.py

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
  n = 6

  runners_str = [
      "Dominique", "Ignace", "Naren", "Olivier", "Philippe", "Pascal"
  ]

  #
  # declare variables
  #
  runners = [model.NewIntVar(1, n, "runners[%i]" % i) for i in range(n)]
  Dominique, Ignace, Naren, Olivier, Philippe, Pascal = runners

  #
  # constraints
  #
  model.AddAllDifferent(runners)

  # a: Olivier not last
  model.Add(Olivier != n)

  # b: Dominique, Pascal and Ignace before Naren and Olivier
  model.Add(Dominique < Naren)
  model.Add(Dominique < Olivier)
  model.Add(Pascal < Naren)
  model.Add(Pascal < Olivier)
  model.Add(Ignace < Naren)
  model.Add(Ignace < Olivier)

  # c: Dominique better than third
  model.Add(Dominique < 3)

  # d: Philippe is among the first four
  model.Add(Philippe <= 4)

  # e: Ignace neither second nor third
  model.Add(Ignace != 2)
  model.Add(Ignace != 3)

  # f: Pascal three places earlier than Naren
  model.Add(Pascal + 3 == Naren)

  # g: Neither Ignace nor Dominique on fourth position
  model.Add(Ignace != 4)
  model.Add(Dominique != 4)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    runners_val = [solver.Value(runners[i]) for i in range(n)]
    print("runners:", runners_val)
    print("Places:")
    for i in range(1, n + 1):
      for j in range(n):
        if runners_val[j] == i:
          print("%i: %s" % (i, runners_str[j]))
    print()

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == '__main__':
  main()
