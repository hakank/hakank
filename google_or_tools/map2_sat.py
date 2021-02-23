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

  Map coloring problem in OR-tools CP-SAT Solver.


  From Pascal Van Hentenryck 'The OPL Optimization Programming Language',
  page 7, 42.

  This is a little more general approach than map_sat.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter



def main():

  model = cp.CpModel()

  #
  # data
  #
  Belgium = 0
  Denmark = 1
  France = 2
  Germany = 3
  Netherlands = 4
  Luxembourg = 5

  n = 6
  max_num_colors = 4

  neighbours = [[France,     Belgium],
                [France,     Luxembourg],
                [France,     Germany],
                [Luxembourg, Germany],
                [Luxembourg, Belgium],
                [Belgium,    Netherlands],
                [Belgium,    Germany],
                [Germany,    Netherlands],
                [Germany,    Denmark]]



  # declare variables
  color = [model.NewIntVar(1, max_num_colors, "x%i" % i) for i in range(n)]

  #
  # constraints
  #
  model.Add(color[Belgium] == 1)  # Symmetry breaking
  for c1,c2 in neighbours:
    model.Add(color[c1] != color[c2])

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = ListPrinter(color)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solutions found")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())

if __name__ == "__main__":
  main()
