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

  Divisible by 9 through 1 puzzle in OR-tools CP-SAT Solver.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  ' Solving Combinatory Problems with LINQ'
  '''
  Find a number consisting of 9 digits in which each of the digits
  from 1 to 9 appears only once. This number must also satisfy these
  divisibility requirements:

   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily
      be divisible by 1).
  '''

  Also, see
  'Intel Parallel Studio: Great for Serial Code Too (Episode 1)'
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/


  This model is however generalized to handle any base, for reasonable limits.
  The 'reasonable limit' for this model is that base must be between 2..16,
  larger than that yield an overflow error.

  This is a port of my old CP model divisible_by_9_through_1.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import toNum


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x, n, base, t, digits_str):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x
        self.__n = n
        self.__base = base
        self.__t = t
        self.__digits_str = digits_str
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("x: ", [self.Value(self.__x[i]) for i in range(self.__n)])
        print("t: ", [self.Value(self.__t[i]) for i in range(self.__n)])
        print("number base 10: %i base %i: %s" % (self.Value(self.__t[0]), self.__base, "".join(
                [self.__digits_str[self.Value(self.__x[i]) + 1] for i in range(self.__n)])))
        print()

    def SolutionCount(self):
        return self.__solution_count


def main(base=10):

  if base > 16:
    print("Sorry, max allowed base is 16. Aborting ...")
    return

  model = cp.CpModel()

  # data
  m = base**(base - 1) - 1
  n = base - 1

  digits_str = "_0123456789ABCDEFGH"

  print("base:", base)

  # declare variables

  # the digits
  x = [model.NewIntVar(1, base - 1, "x[%i]" % i) for i in range(n)]

  # the numbers, t[0] contains the answer
  t = [model.NewIntVar(0, m, "t[%i]" % i) for i in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(x)

  for i in range(n):
    mm = base - i - 1
    toNum(model, [x[j] for j in range(mm)], t[i], base)
    model.AddModuloEquality(0, t[i], mm)


  #
  # solution and search
  #
  solver = cp.CpSolver() 
  # status = solver.Solve(model)
  solution_printer = SolutionPrinter(x, n, base, t, digits_str)
  status = solver.SearchForAllSolutions(model, solution_printer)

  # if status == cp.OPTIMAL:
  #   print("x: ", [solver.Value(x[i]) for i in range(n)])
  #   print("t: ", [solver.Value(t[i]) for i in range(n)])
  #   print("number base 10: %i base %i: %s" % (solver.Value(t[0]), base, "".join(
  #       [digits_str[solver.Value(x[i]) + 1] for i in range(n)])))
  #   print()

  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


base = 10
default_base = 10
max_base = 16
if __name__ == "__main__":
  # if len(sys.argv) > 1:
  #   base = int(sys.argv[1])
  #   if base > max_base:
  #     print("Sorry, max allowed base is %i. Setting base to %i..." %
  #           (max_base, default_base))
  #     base = default_base
  # main(base)

  for base in range(2, 17):
      print()
      main(base)
