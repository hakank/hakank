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
  
  Added corner puzzle in OR-tools CP-SAT Solver.

  Added corner puzzle in Z3

  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  '''
  This puzzle requires that you enter the digits 1 through 8 in the circles and
  squares (one digit in each figure) so that the number in each square is equal
  to the sum on the numbers in the circles which adjoin it.  
  ...

     C F C
     F   F
     C F C
  '''

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        x = self.__x 
        print("%s %s %s" % (self.Value(x[0]), self.Value(x[1]),self.Value(x[2])))
        print("%s %s %s" % (self.Value(x[3]), ' ',             self.Value(x[4])))
        print("%s %s %s" % (self.Value(x[5]), self.Value(x[6]),self.Value(x[7])))
        print()

    def SolutionCount(self):
        return self.__solution_count

def main():

  model = cp.CpModel()

  n = 8
  
  x = [model.NewIntVar(1,n,f"x[{i}]") for i in range(n)]
  a,b,c,d,e,f,g,h = x
  
  # constraints
  model.AddAllDifferent(x)
  model.Add(b == a + c)
  model.Add(d == a + f)
  model.Add(e == c + h)
  model.Add(g == f + h)


  solver  = cp.CpSolver()
  
  solution_printer = SolutionPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)
  ret = []
  if status in [cp.OPTIMAL, cp.FEASIBLE]:
    ret = [i for i in range(n) if solver.Value(x[i]) == 1]
  else:
    print("No solution!")
    ret = [-1 for _i in range(m)]

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()

  return ret

if __name__ == '__main__':
  print("return values from main:", main())
