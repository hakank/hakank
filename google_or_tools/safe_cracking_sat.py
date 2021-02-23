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
  Safe cracking puzzle in OR-tools CP-SAT Solver.

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  '''
  The code of Professor Smart's safe is a sequence of 9 distinct
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

   C1 <> 1, C2 <> 2, ..., C9 <> 9

  can you find the correct combination?
  '''

  This is a port of my old CP model safe_cracking.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import prod

def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 9

  #
  # variables
  #

  LD = [model.NewIntVar(1,9, 'LD[%i]' % i) for i in range(n)]
  C1, C2, C3, C4, C5, C6, C7, C8, C9 = LD

  #
  # constraints
  #
  model.AddAllDifferent(LD)

  model.Add(C4 - C6 == C7)
  # model.Add(C1 * C2 * C3 == C8 + C9)
  C8C9 = model.NewIntVar(0,18,"C8C9")
  model.Add(C8C9 == C8 + C9)
  C1C2C3 = model.NewIntVar(0,27,"C1C2C3")
  prod(model,[C1,C2,C3],C1C2C3),
  model.Add(C8C9 == C1C2C3)
  model.Add(C2 + C3 + C6 < C8)
  model.Add(C9 < C8)
  for i in range(n):
    model.Add(LD[i] != i + 1)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('LD:', [solver.Value(LD[i]) for i in range(n)])

  print()
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
