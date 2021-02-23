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

  Pandigital numbers in OR-tools CP-SAT Solver.

  From Albert H. Beiler 'Recreations in the Theory of Numbers',
  quoted from http://www.worldofnumbers.com/ninedig1.htm
  '''
  Chapter VIII : Digits - and the magic of 9

  The following curious table shows how to arrange the 9 digits so that
  the product of 2 groups is equal to a number represented by the
  remaining digits.

     12 x 483 = 5796
     42 x 138 = 5796
     18 x 297 = 5346
     27 x 198 = 5346
     39 x 186 = 7254
     48 x 159 = 7632
     28 x 157 = 4396
     4 x 1738 = 6952
     4 x 1963 = 7852
  '''

  See also MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
  '''
  A number is said to be pandigital if it contains each of the digits
  from 0 to 9 (and whose leading digit must be nonzero). However,
  'zeroless' pandigital quantities contain the digits 1 through 9.
  Sometimes exclusivity is also required so that each digit is
  restricted to appear exactly once.
  '''

  * Wikipedia http://en.wikipedia.org/wiki/Pandigital_number

  This is a port of my old CP model pandigital_numbers.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import toNum


def print_solution(solver, x, len1, len2, x_len):
  print("".join([str(solver.Value(x[i])) for i in range(len1)]), "*", end=" ")
  print("".join([str(solver.Value(x[i])) for i in range(len1, len1 + len2)]), "=", end=" ")
  print("".join([str(solver.Value(x[i])) for i in range(len1 + len2, x_len)]))


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x, len1, len2, x_len):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x
        self.__len1 = len1
        self.__len2 = len2
        self.__x_len = x_len
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print_solution(self, self.__x, self.__len1, self.__len2, self.__x_len)        

    def SolutionCount(self):
        return self.__solution_count


def main(base=10, start=1, len1=1, len2=4):

  model = cp.CpModel()

  #
  # data
  #
  max_d = base - 1
  x_len = max_d + 1 - start
  max_num = base**4 - 1

  #
  # declare variables
  #
  num1 = model.NewIntVar(0, max_num, "num1")
  num2 = model.NewIntVar(0, max_num, "num2")
  res = model.NewIntVar(0, max_num, "res")

  x = [model.NewIntVar(start, max_d, "x[%i]" % i) for i in range(x_len)]

  #
  # constraints
  #
  model.AddAllDifferent(x)

  toNum(model, [x[i] for i in range(len1)], num1, base)
  toNum(model, [x[i] for i in range(len1, len1 + len2)], num2, base)
  toNum(model, [x[i] for i in range(len1 + len2, x_len)], res, base)

  # model.Add(num1 * num2 == res)
  model.AddMultiplicationEquality(res,[num1, num2])

  # no number must start with 0
  model.Add(x[0] > 0)
  model.Add(x[len1] > 0)
  model.Add(x[len1 + len2] > 0)

  # symmetry breaking
  model.Add(num1 < num2)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(x, len1, len2, x_len)
  _status = solver.SearchForAllSolutions(model, solution_printer)

  return solution_printer.SolutionCount()
  # if 0 and num_solutions > 0:
  #   print()
  #   print("NumSolutions:", solution_printer.SolutionCount())
  #   print("NumConflicts:", solver.NumConflicts())
  #   print("NumBranches:", solver.NumBranches())
  #   print("WallTime:", solver.WallTime())
  #   print()


base = 10
start = 1
if __name__ == "__main__":
  if len(sys.argv) > 1:
    base = int(sys.argv[1])
  if len(sys.argv) > 2:
    start = int(sys.argv[2])

  num_sols = 0
  x_len = base - 1 + 1 - start
  for len1 in range(1 + (x_len)):
    for len2 in range(1 + (x_len)):
      if x_len > len1 + len2:
        num_sols += main(base, start, len1, len2)
  print("\nNumSolutions:", num_sols)
