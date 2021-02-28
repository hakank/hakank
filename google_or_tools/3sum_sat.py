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
  
  3SUM (Three Elements That Sum To Zero) in OR-tools CP-SAT Solver.

  From
  http://nathanleclaire.com/blog/2013/10/22/three-elements-that-sum-to-zero/
  '''
  Given a collection of integers, return the indices of any three elements which sum to zero. 
  For instance, if you are given {-1, 6, 8, 9, 10, -100, 78, 0, 1}, you could return {0, 7, 8} 
  because -1 + 1 + 0 == 0. You can't use the same index twice, and if there is no match you 
  should return {-1, -1, -1}.
  '''

  Also see: https://en.wikipedia.org/wiki/3SUM
   
  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, nums, n, x):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__nums = nums
        self.__n = n
        self.__x = x
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        n = self.__n 
        x = self.__x 
        print("x:", [self.Value(x[i]) for i in range(n)])
        ixs = [i for i in range(n) if self.Value(x[i]) == 1]
        print("indices:", ixs)
        print("numbers used:", [self.__nums[i] for i in ixs])
        print()

    def SolutionCount(self):
        return self.__solution_count

def main():

  model = cp.CpModel()

  
  nums = [-1, 6, 8, 9, 10, -100, 78, 0, 1]
  # nums = [1, 6, 8, 9, 10, 100, 78, 0, 1] # UNSAT
  n = len(nums)

  m = 3 # the number of elements that should sum to 0

  x = [model.NewBoolVar(f"x[{i}]") for i in range(n)]
  
  
  model.Add(sum([nums[i]*x[i] for i in range(n)]) == 0)
  model.Add(sum(x) == m)


  solver  = cp.CpSolver()
  
  solution_printer = SolutionPrinter(nums, n,x)
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
