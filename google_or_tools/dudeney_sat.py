# Copyright 2010 Pierre Schaus (pschaus@gmail.com) Original model
# Copyright 2021 Hakan Kjellerstrand (hakank@gmail.com) Current model
# Licensed under the Apache License, Version 2.0 (the 'License');
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an 'AS IS' BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""
  Dudeney's Number problem in OR-tools CP-SAT Solver

  From Pierre Schaus' blog post
  "Dudeney number"
  http://cp-is-fun.blogspot.com/2010/09/test-python.html
  '''
  I discovered yesterday Dudeney Numbers
  A Dudeney Numbers is a positive integer that is a perfect cube such that the sum 
  of its decimal digits is equal to the cube root of the number. There are only six 
  Dudeney Numbers and those are very easy to find with CP.
  I made my first experience with google cp solver to find these numbers (model below) 
  and must say that I found it very convenient to build CP models in python!
  When you take a close look at the line: 
      solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
  It is difficult to argue that it is very far from dedicated 
  optimization languages!
  '''
  
  Also see: http://en.wikipedia.org/wiki/Dudeney_number

  This is a port of Pierre Schaus old CP model dudeney.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x, nb, s):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x 
        self.__nb = nb
        self.__s = s
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("x:", [self.Value(v) for v in self.__x])
        print("nb:", self.Value(self.__nb), "s:", self.Value(self.__s))
        print()

    def SolutionCount(self):
        return self.__solution_count


def dudeney(n=6):
  
  model = cp.CpModel()

  x = [model.NewIntVar(0,9, 'x' + str(i)) for i in range(n)]
  nb = model.NewIntVar(0, 10 ** n, 'nb') # Number
  s = model.NewIntVar(1, 9 * n + 1, 's') # Digit sum

  # This don't work since the current AddMultiplicationEquality
  # don't handle more than two values.
  # model.AddMultiplicationEquality(nb,[s,s,s])
  # """
  # Check failed: vars.size() == 2 (3 vs. 2) General int_prod not supported yet.
  # *** Check failure stack trace: ***
  # """

  # Work-around:
  s2 = model.NewIntVar(1, (9 * n + 1)**2, 's2')
  model.AddMultiplicationEquality(s2,[s,s])
  model.AddMultiplicationEquality(nb,[s,s2])

  model.Add(sum([10 ** (n - i - 1) * x[i] for i in range(n)]) == nb)
  model.Add(sum([x[i] for i in range(n)]) == s)

  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(x, nb, s)
  status = solver.SearchForAllSolutions(model,solution_printer)

  if status != cp.OPTIMAL:
    print("No solution!")
    

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
 


if __name__ == '__main__':
  dudeney(6)
