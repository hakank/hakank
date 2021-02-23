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

  Costas array in OR-tools CP-SAT Solver.

  From http://mathworld.wolfram.com/CostasArray.html:
  '''
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  '''

  Also see
  http://en.wikipedia.org/wiki/Costas_array

  About this model:
  This model is based on Barry O'Sullivan's model:
  http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn

  and my small changes in
  http://hakank.org/minizinc/costas_array.mzn

  Since there is no symmetry breaking of the order of the Costas
  array it gives all the solutions for a specific length of
  the array, e.g. those listed in
  http://mathworld.wolfram.com/CostasArray.html

  1     1       (1)
  2     2       (1, 2), (2,1)
  3     4       (1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
  4     12      (1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4),
                (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1),
                (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
  ....

  See http://www.research.att.com/~njas/sequences/A008404
  for the number of solutions for n=1..
  1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828,
  17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052,
  872, 200, 88, 56, 204,...

  This is a port of my old CP solver model costas_array.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, costas, differences):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__costas = costas 
        self.__differences = differences
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("costas:", [self.Value(self.__costas[i]) for i in range(n)])
        print("differences:")
        for i in range(n):
          for j in range(n):
            v = self.Value(self.__differences[i, j])
            if v == -n + 1:
              print("  ", end=" ")
            else:
              print("%2d" % v, end=" ")
          print()
        print()

    def SolutionCount(self):
        return self.__solution_count



def main(n=6):

  model = cp.CpModel() 

  #
  # data
  #
  print("n:", n)

  #
  # declare variables
  #
  costas = [model.NewIntVar(1, n, "costas[%i]" % i) for i in range(n)]
  differences = {}
  for i in range(n):
    for j in range(n):
      differences[(i, j)] = model.NewIntVar(-n + 1, n - 1,
                                          "differences[%i,%i]" % (i, j))

  #
  # constraints
  #

  # Fix the values in the lower triangle in the
  # difference matrix to -n+1. This removes variants
  # of the difference matrix for the the same Costas array.
  for i in range(n):
    for j in range(i + 1):
      model.Add(differences[i, j] == -n + 1)

  # hakank: All the following constraints are from
  # Barry O'Sullivans's original model.
  #
  model.AddAllDifferent(costas)

  # "How do the positions in the Costas array relate
  #  to the elements of the distance triangle."
  for i in range(n):
    for j in range(n):
      if i < j:
        model.Add(differences[(i, j)] == costas[j] - costas[j - i - 1])

  # "All entries in a particular row of the difference
  #  triangle must be distint."
  for i in range(n - 2):
    model.AddAllDifferent([differences[i, j] for j in range(n) if j > i])

  #
  # "All the following are redundant - only here to speed up search."
  #

  # "We can never place a 'token' in the same row as any other."
  for i in range(n):
    for j in range(n):
      if i < j:
        model.Add(differences[i, j] != 0)

  for k in range(2, n):
    for l in range(2, n):
      if k < l:
        model.Add(differences[k - 2, l - 1] + differences[k, l] ==
                   differences[k - 1, l - 1] + differences[k - 1, l])

  #
  # search and result
  #
  solver = cp.CpSolver() 
  # status = solver.Solve(model)
  solution_printer = SolutionPrinter(costas, differences)
  status = solver.SearchForAllSolutions(model,solution_printer)

  if status == cp.OPTIMAL:
    print("costas:", [solver.Value(costas[i]) for i in range(n)])
    print("differences:")
    for i in range(n):
      for j in range(n):
        v = solver.Value(differences[i, j])
        if v == -n + 1:
          print("  ", end=" ")
        else:
          print("%2d" % v, end=" ")
      print()
    print()

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


n = 6
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  main(n)
