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
  Scheduling speakers problem in OR-tools CP-SAT Solver.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  This is a port of my old CP model scheduling_speakers.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import *


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 6  # number of speakers

  # slots available to speak
  available = [
      # Reasoning:
      [3, 4, 5, 6],  # 2) the only one with 6 after speaker F -> 1
      [3, 4],  # 5) 3 or 4
      [2, 3, 4, 5],  # 3) only with 5 after F -> 1 and A -> 6
      [2, 3, 4],  # 4) only with 2 after C -> 5 and F -> 1
      [3, 4],  # 5) 3 or 4
      [1, 2, 3, 4, 5, 6]  # 1) the only with 1
  ]

  #
  # variables
  #
  x = [model.NewIntVar(1, n, 'x[%i]' % i) for i in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(x)

  for i in range(n):
    # solver.Add(solver.MemberCt(x[i], available[i]))
    memberOf(model,available[i], x[i])

  #
  # search and result
  #
  solver = cp.CpSolver()
  solution_printer = ListPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if status != cp.OPTIMAL:
    print("No solution")


  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
