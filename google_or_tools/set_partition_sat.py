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

  Set partition problem in OR-tools CP-SAT Solver.

  Problem formulation from
  http://www.koalog.com/resources/samples/PartitionProblem.java.html
  '''
   This is a partition problem.
   Given the set S = {1, 2, ..., n},
   it consists in finding two sets A and B such that:

     A U B = S,
     |A| = |B|,
     sum(A) = sum(B),
     sum_squares(A) = sum_squares(B)

  '''

  This model uses a binary matrix to represent the sets.

  This is a port of my old CP model set_partition.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, num_sets, n, a):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__num_sets = num_sets
        self.__n = n
        self.__a = a
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        a_val = {}
        for i in range(self.__num_sets):
          for j in range(self.__n):
            a_val[i, j] = self.Value(self.__a[i, j])

        sq = sum([(j + 1) * a_val[0, j] for j in range(n)])
        print("sums:", sq)
        sq2 = sum([((j + 1) * a_val[0, j])**2 for j in range(n)])
        print("sums squared:", sq2)

        for i in range(num_sets):
          if sum([a_val[i, j] for j in range(n)]):
            print(i + 1, ":", end=" ")
            for j in range(n):
              if a_val[i, j] == 1:
                print(j + 1, end=" ")
            print()
        print()

    def SolutionCount(self):
        return self.__solution_count



#
# Partition the sets (binary matrix representation).
#
def partition_sets(model, x, num_sets, n):
  for i in range(num_sets):
    for j in range(i):
      if i != j:
        # model.Add(sum([x[i, k] *  for k in range(n)]) == 0) # old
        s = [model.NewIntVar(0,1, "s") for k in range(n)]
        for k in range(n):
          model.AddMultiplicationEquality(s[k],[x[i, k],x[j, k]])
        model.Add(sum(s) == 0)

  # ensure that all integers is in
  # (exactly) one partition
  model.Add(sum([x[i, j] for i in range(num_sets) for j in range(n)]) == n)


def main(n=16, num_sets=2):

  model = cp.CpModel()

  #
  # data
  #
  print("n:", n)
  print("num_sets:", num_sets)
  print()

  # Check sizes
  assert n % num_sets == 0, "Equal sets is not possible."

  #
  # variables
  #

  # the set
  a = {}
  for i in range(num_sets):
    for j in range(n):
      a[i, j] = model.NewIntVar(0, 1, "a[%i,%i]" % (i, j))

  #
  # constraints
  #

  # partition set
  partition_sets(model, a, num_sets, n)

  for i in range(num_sets):
    for j in range(i, num_sets):

      # same cardinality
      model.Add(
          sum([a[i, k] for k in range(n)]) == 
          sum([a[j, k] for k in range(n)]))

      # same sum
      model.Add(
          sum([k * a[i, k] for k in range(n)]) == 
          sum([k * a[j, k] for k in range(n)]))

      # same sum squared
      # old:
      # model.Add(
      #   sum([(k * a[i, k]) * (k * a[i, k]) for k in range(n)]) ==
      #   sum([(k * a[j, k]) * (k * a[j, k]) for k in range(n)]))
      sq1 = []
      sq2 = []
      for k in range(n):
        v1 = model.NewIntVar(0,n*n,"v1")
        t1 = model.NewIntVar(0,n*n,"t1")
        model.Add(t1 == (k * a[i, k]))
        model.AddMultiplicationEquality(v1,[t1,t1])
        sq1.append(v1)

        v2 = model.NewIntVar(0,n*n,"v2")
        t2 = model.NewIntVar(0,n*n,"t2")
        model.Add(t2 == (k * a[j, k]))
        model.AddMultiplicationEquality(v2,[t2,t2])
        sq2.append(v2)

      model.Add(sum(sq1) == sum(sq2))

  # symmetry breaking for num_sets == 2
  if num_sets == 2:
    model.Add(a[0, 0] == 1)

  #
  # search and result
  #
  solver = cp.CpSolver()
  solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  # solver.parameters.cp_model_probing_level = 0

  solution_printer = SolutionPrinter(num_sets, n, a)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if status != cp.OPTIMAL:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


n = 16
num_sets = 2
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  if len(sys.argv) > 2:
    num_sets = int(sys.argv[2])

  main(n, num_sets)
