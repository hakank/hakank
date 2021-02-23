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

  Olympic puzzle in OR-tools CP-SAT Solver.

  Benchmark for Prolog (BProlog)
  '''
  File   : olympic.pl
  Author : Neng-Fa ZHOU
  Date   : 1993

  Purpose: solve a puzzle taken from Olympic Arithmetic Contest

  Given ten variables with the following configuration:

                 X7   X8   X9   X10

                    X4   X5   X6

                       X2   X3

                          X1

  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables
                        Xi   Xj

                           Xk
  the following constraint is satisfied:

                      |Xi-Xj| = Xk
  '''

  This is a port of my old CP model olympic.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter


def minus(model, x, y, z):
  _lb,ub = x.Proto().domain
  
  v = model.NewIntVar(-ub,ub,"v")
  model.Add(v == x - y)
  model.AddAbsEquality(z,v)


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 10

  #
  # declare variables
  #
  Vars = [model.NewIntVar(1, n, 'Vars[%i]' % i) for i in range(n)]
  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10 = Vars

  #
  # constraints
  #
  model.AddAllDifferent(Vars)

  model.Add(X1 == 3)
  minus(model, X2, X3, X1)
  minus(model, X4, X5, X2)
  minus(model, X5, X6, X3)
  minus(model, X7, X8, X4)
  minus(model, X8, X9, X5)
  minus(model, X9, X10, X6)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = ListPrinter(Vars)
  _status = solver.SearchForAllSolutions(model,solution_printer)

  # if status == cp.OPTIMAL:
  #  print('Vars:', [solver.Value(Vars[i]) for i in range(n)])

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
