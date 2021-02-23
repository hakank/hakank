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

  Volsay problem in OR-tools CP-SAT Solver.

  From the OPL model volsay.mod

  This is a port of my old LP model volsay.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main():

  model = cp.CpModel() 

  # data

  # declare variables
  Gas = model.NewIntVar(0, 100000, 'Gas')
  Chloride = model.NewIntVar(0, 100000, 'Cloride')

  #
  # constraints
  #
  model.Add(Gas + Chloride <= 50)
  model.Add(3 * Gas + 4 * Chloride <= 180)

  # objective
  model.Maximize(40 * Gas + 50 * Chloride)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print()
    print('objective = ', solver.ObjectiveValue())
    print('Gas = ', solver.Value(Gas))
    print('Chloride:', solver.Value(Chloride))

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


if __name__ == '__main__':
  main()
