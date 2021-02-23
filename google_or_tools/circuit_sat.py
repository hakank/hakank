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

  Decomposition of the circuit constraint in Google CP Solver.

  Cf Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html

  Solution of n=4:
  x: [2, 0, 3, 1]
  x: [3, 0, 1, 2]
  x: [1, 3, 0, 2]
  x: [3, 2, 0, 1]
  x: [1, 2, 3, 0]
  x: [2, 3, 1, 0]

  The 'orbit' method that is used here is based on some
  observations on permutation orbits.

  This is a port of my old OR-tools CP model circuit.py


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google OR-tools models: http://www.hakank.org/or_tools/

"""

from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import circuit, ListPrinter


def main(n=5):

  model = cp.CpModel() 

  # data
  print("n:", n)

  # declare variables
  # Note: domain should be 0..n-1
  x = [model.NewIntVar(0, n - 1, "x%i" % i) for i in range(n)]

  #
  # constraints
  #
  circuit(model, x)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  solver.parameters.cp_model_presolve=False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  # status = solver.Solve(model)
  solution_printer = ListPrinter(x)
  _status = solver.SearchForAllSolutions(model, solution_printer)

  # if status == cp.OPTIMAL:
  #  print("x:", [solver.Value(x[i]) for i in range(len(x))])

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBanches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


n = 5
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])

  main(n)
