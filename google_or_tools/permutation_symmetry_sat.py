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
  
  Permutation symmetry in OR-tools CP-SAT Solver.

  (This is a port of my original MiniZinc model 
   http://hakank.org/minizinc/permutation_symmetry.mzn )

  https://stackoverflow.com/questions/66385872/minizinc-constraint-programming-enforce-interchangeability-of-items
  '''
  Minizinc (Constraint programming) enforce interchangeability of items

  I'm using Minizinc to do constraint programming.

  The problem is as follows:

  I have an array of colors, denoted as ints. So this looks like: 
     [3, 5, 5, 2, 1, 3, 1].

  This array should be optimized in such a way that the changes of 
  colors is minimized.

  An optimal solution for example would be: [1, 1, 3, 3, 2, 5, 5].

  I also have an order array, which is used to optimize with. This 
  array denotes the sequence in which the coloring will be done. So 
  the value of each x_i in the order array, denotes the index in the 
  optimal sequence. So the optimal solution, the order array would be:

    [7, 5, 1, 6, 4, 2, 3]

  For this small example it is pretty trivial how to write the proper 
  constraints. However, if N gets large, the solver will try many 
  solutions that are symmetrical. So in order to reduce the number 
  of possibile solutions, I need to tell the solver that an order 
  array is symmetrical if it results in the same color sequencing. 
  Or, in other words, that colors are interchangeable.

  My question is how to construct such a constraint?
  '''

  Hmm, for some reason, the user Simon deleted the question after
  I answered it. Why?

  My answer was:
  '''
  One idea to do symmetry breaking is the following (see model below): 
  for equal values in the optimal list enforce that the permutation index 
  of the first value is less than the permutation index for the second 
  value.

  I've also included the predicate permutation3 which calculates 
  the permutation given the a "source" array and a "sink" array.

  What I understand from your description, optimal is given from some 
  other part of the model so it's shown as fixed here.

  <the MiniZinc model> 

  Instead of 8 solutions, it outputs the unique solution:

  colors: [3, 5, 5, 2, 1, 3, 1]
  perm: [5, 7, 1, 6, 4, 2, 3]
  optimal: [1, 1, 3, 3, 2, 5, 5]
  ----------
  ==========
  '''

  The solution of this model (with symmetry breaking) is:

    colors :  [3, 5, 5, 2, 1, 3, 1]
    perm   :  [6, 4, 5, 0, 3, 2, 1]
    optimal:  [1, 1, 3, 3, 2, 5, 5]

  The permutation is 0-based, whereas MiniZinc is 
  1-based (by default).

  Without symmetry breaking there are 8 solutions.
  Here are the permutations for the different solutions:

    perm   :  [6, 4, 0, 5, 3, 1, 2]
    perm   :  [6, 4, 5, 0, 3, 1, 2]
    perm   :  [6, 4, 0, 5, 3, 2, 1]
    perm   :  [6, 4, 5, 0, 3, 2, 1]
    perm   :  [4, 6, 0, 5, 3, 1, 2]
    perm   :  [4, 6, 0, 5, 3, 2, 1]
    perm   :  [4, 6, 5, 0, 3, 1, 2]
    perm   :  [4, 6, 5, 0, 3, 2, 1]

  For example, in perm[0] and perm[1] there are the values
  6 and 4 which corresponds to the positions of the two 1s 
  in the colors array. There are 4 such duplicates in
  the colors array (and the optimal array) : 1, 2, 3, and 5,
  thus there are 
    2 * 4 == 8 
  different solutions without symmetry breaking.


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import permutation3

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, colors, perm, optimal):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__colors = colors
        self.__perm = perm
        self.__optimal = optimal
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("colors : ", self.__colors)
        print("perm   : ", [self.Value(v) for v in self.__perm])
        print("optimal: ", [self.Value(v) for v in self.__optimal])
        print()

    def SolutionCount(self):
        return self.__solution_count


def main():

  model = cp.CpModel()

  colors = [3, 5, 5, 2, 1, 3, 1]
  n = len(colors)
  perm = [model.NewIntVar(0,n-1,f"perm[{i}") for i in range(n)]

  # This is calculated in other parts of the OP's model so
  # it's hard coded below.
  optimal = [model.NewIntVar(0,n-1,f"optimal[{i}") for i in range(n)]

  # The hard coded values of optimal
  optimal_val = [1, 1, 3, 3, 2, 5, 5]
  for i in range(n):
    model.Add(optimal[i] == optimal_val[i])

  # Create the permutation perm from colors -> optimal
  permutation3(model, colors, perm, optimal)

  #
  # Symmetry breaking (the "meat" of the model)
  #
  for i in range(n):
    for j in range(i):
      # optimal[i] == optimal[j] => perm[i] < perm[j]
      b_opt = model.NewBoolVar("b_opt")
      model.Add(optimal[i] == optimal[j]).OnlyEnforceIf(b_opt)
      model.Add(optimal[i] != optimal[j]).OnlyEnforceIf(b_opt.Not())

      b_perm = model.NewBoolVar("b_perm")
      model.Add(perm[i] < perm[j]).OnlyEnforceIf(b_perm)
      model.Add(perm[i] > perm[j]).OnlyEnforceIf(b_perm.Not())

      # Comment this line to get the 8 solutions
      # without symmetry breaking.
      model.AddImplication(b_opt,b_perm)


  solver  = cp.CpSolver()
  solution_printer = SolutionPrinter(colors,perm,optimal)
  status = solver.SearchForAllSolutions(model, solution_printer)
  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())

if __name__ == '__main__':
  main()
