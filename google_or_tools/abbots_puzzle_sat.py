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
  
  Abbot's puzzle in OR-tools CP-SAT Solver.

  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  '''
  The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.
  
  If 100 bushels of corn were distributed among 100 people in such a
  manner that each man received three bushels, each woman two, and each
  child half a bushel, how many men, women, and children were there?
  
  Dudeney added the condition that there are five times as many women as
  men. That way, the solution becomes unique (otherwise, there are seven
  solutions).
  '''
  
   
  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print([self.Value(v) for v in self.__x])

    def SolutionCount(self):
        return self.__solution_count

def main():

  model = cp.CpModel()

  x = [model.NewIntVar(0, 100, f"x[{i}]") for i in range(3)]
  M, W, C = x

  model.Add(100 == M + W + C)
  model.Add(M * 6 + W * 4 + C == 200)
  model.Add(M * 5 == W)

  solver  = cp.CpSolver()
  
  solution_printer = SolutionPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)
  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()

if __name__ == '__main__':
  main()
