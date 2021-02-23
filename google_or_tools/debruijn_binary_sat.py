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

  de Bruijn sequences in OR-tools CP-SAT Solver.

  Implementation of de Bruijn sequences in Minizinc, both 'classical' and
  'arbitrary'.
  The 'arbitrary' version is when the length of the sequence (m here) is <
  base**n.


  Compare with the the web based programs:
    http://www.hakank.org/comb/debruijn.cgi
    http://www.hakank.org/comb/debruijn_arb.cgi
    http://hakank.org/javascript_progs/debruijn.html

  This is a port of my old CP model debruijn_binary.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import toNum, count_vars





class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, x, gcc, bin_code):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x 
        self.__gcc = gcc 
        self.__bin_code = bin_code
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print(f"Solution #{self.__solution_count}")
        print("x:", [self.Value(self.__x[i]) for i in range(m)])
        print("gcc:", [self.Value(self.__gcc[i]) for i in range(base)])
        print("de Bruijn sequence:", [self.Value(self.__bin_code[i]) for i in range(m)])
        print()

    def SolutionCount(self):
        return self.__solution_count


def main(base=2, n=3, m=8):

  model = cp.CpModel()

  #
  # data
  #
  # base = 2  # the base to use, i.e. the alphabet 0..n-1
  # n    = 3  # number of bits to use (n = 4 -> 0..base^n-1 = 0..2^4 -1, i.e. 0..15)
  # m    = base**n  # the length of the sequence. For "arbitrary" de Bruijn
  # sequences

  # base = 4
  # n    = 4
  # m    = base**n

  # harder problem
  #base = 13
  #n = 4
  #m = 52

  # for n = 4 with different value of base
  # base = 2  0.030 seconds  16 failures
  # base = 3  0.041         108
  # base = 4  0.070         384
  # base = 5  0.231        1000
  # base = 6  0.736        2160
  # base = 7  2.2 seconds  4116
  # base = 8  6 seconds    7168
  # base = 9  16 seconds  11664
  # base = 10 42 seconds  18000
  # base = 6
  # n = 4
  # m = base**n

  # if True then ensure that the number of occurrences of 0..base-1 is
  # the same (and if m mod base = 0)
  check_same_gcc = True

  print("base: %i n: %i m: %i" % (base, n, m))
  if check_same_gcc:
    print("Checks gcc")

  # declare variables
  x = [model.NewIntVar(0, (base**n) - 1, "x%i" % i) for i in range(m)]
  binary = {}
  for i in range(m):
    for j in range(n):
      binary[(i, j)] = model.NewIntVar(0, base - 1, "x_%i_%i" % (i, j))

  bin_code = [model.NewIntVar(0, base - 1, "bin_code%i" % i) for i in range(m)]

  #
  # constraints
  #
  #model.Add(solver.AllDifferent([x[i] for i in range(m)]))
  model.AddAllDifferent(x)

  # converts x <-> binary
  for i in range(m):
    t = [model.NewIntVar(0, base - 1, "t_%i" % j) for j in range(n)]
    toNum(model, t, x[i], base)
    for j in range(n):
      model.Add(binary[(i, j)] == t[j])

  # the de Bruijn condition
  # the first elements in binary[i] is the same as the last
  # elements in binary[i-i]
  for i in range(1, m - 1):
    for j in range(1, n - 1):
      model.Add(binary[(i - 1, j)] == binary[(i, j - 1)])

  # ... and around the corner
  for j in range(1, n):
    model.Add(binary[(m - 1, j)] == binary[(0, j - 1)])

  # converts binary -> bin_code
  for i in range(m):
    model.Add(bin_code[i] == binary[(i, 0)])

  # extra: ensure that all the numbers in the de Bruijn sequence
  # (bin_code) has the same occurrences (if check_same_gcc is True
  # and it is mathematically possible)
  gcc = [model.NewIntVar(0, m, "gcc%i" % i) for i in range(base)]
  if check_same_gcc and m % base == 0:
    for i in range(1, base):
      count_vars(model, bin_code, i, gcc[i] )
      model.Add(gcc[i] == gcc[i - 1])

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0


  solution_printer = SolutionPrinter(x, gcc, bin_code)
  status = solver.SearchForAllSolutions(model, solution_printer)
  # status = solver.Solve(model)
  
  # if status == cp.OPTIMAL:
  #   print("x:", [solver.Value(x[i]) for i in range(m)])
  #   print("gcc:", [solver.Value(gcc[i]) for i in range(base)])
  #   print("de Bruijn sequence:", [solver.Value(bin_code[i]) for i in range(m)])
  #   # for i in range(m):
  #   #    for j in range(n):
  #   #        print binary[(i,j)].Value(),
  #   #    print
  #   # print

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


base = 2
n = 3
m = base**n
if __name__ == "__main__":
  if len(sys.argv) > 1:
    base = int(sys.argv[1])
  if len(sys.argv) > 2:
    n = int(sys.argv[2])
  if len(sys.argv) > 3:
    m = int(sys.argv[3])
  else:
    m = base**n

  main(base, n, m)
