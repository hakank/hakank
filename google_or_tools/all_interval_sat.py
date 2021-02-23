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

  All interval problem in Google CP Solver.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  '''
  Given the twelve standard pitch-classes (c, c , d, ...), represented by
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly
  once and in which the musical intervals between neighbouring notes cover
  the full set of intervals from the minor second (1 semitone) to the major
  seventh (11 semitones). That is, for each of the intervals, there is a
  pair of neigbhouring pitch-classes in the series, between which this
  interval appears. The problem of finding such a series can be easily
  formulated as an instance of a more general arithmetic problem on Z_n,
  the set of integer residues modulo n. Given n in N, find a vector
  s = (s_1, ..., s_n), such that (i) s is a permutation of
  Z_n = {0,1,...,n-1}; and (ii) the interval vector
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is
  called an all-interval series of size n; the problem of finding such
  a series is the all-interval series problem of size n. We may also be
  interested in finding all possible series of a given size.
  '''

  This is a port of my old CP model all_interval.py
 
  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google OR-tools models: http://www.hakank.org/or_tools/

"""

from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import array_values


class SolutionPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, n, x, diffs):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__x = x
        self.__diffs = diffs
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("x:", array_values(self,self.__x))
        print("diffs:", array_values(self,self.__diffs))
        print()

    def SolutionCount(self):
        return self.__solution_count

def main(n=12):

  # Create the solver.
  model = cp.CpModel()

  # data
  print("n:", n)

  # declare variables
  x = [model.NewIntVar(1, n, "x[%i]" % i) for i in range(n)]
  diffs = [model.NewIntVar(1, n - 1, "diffs[%i]" % i) for i in range(n - 1)]

  # constraints
  model.AddAllDifferent(x)
  model.AddAllDifferent(diffs)

  diffs_v = [model.NewIntVar(-n,n,f"v[{k}]") for k in range(n-1)]
  for k in range(n - 1):
    # diffs[k] == abs(x[k + 1] - x[k])
    model.Add(diffs_v[k]== x[k + 1] - x[k])
    model.AddAbsEquality(diffs[k],diffs_v[k])

  # symmetry breaking
  model.Add(x[0] < x[n - 1])
  model.Add(diffs[0] < diffs[1])

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  # solution_printer = SimpleSolutionCounter(x)
  solution_printer = SolutionPrinter(n,x,diffs)
  status = solver.SearchForAllSolutions(model, solution_printer)

  print('Solutions found : %i' % solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


n = 12
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  main(n)
