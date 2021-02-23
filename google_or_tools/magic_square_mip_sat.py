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

  Magic square (integer programming approach) in OR-tools CP-SAT solver.

  Translated from GLPK:s example magic.mod
  '''
  MAGIC, Magic Square

  Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>

  In recreational mathematics, a magic square of order n is an
  arrangement of n^2 numbers, usually distinct integers, in a square,
  such that n numbers in all rows, all columns, and both diagonals sum
  to the same constant. A normal magic square contains the integers
  from 1 to n^2.

  (From Wikipedia, the free encyclopedia.)
  '''

  This is a port of my old LP model magic_square_mip.py
  

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

#
# main(n, use_output_matrix)
#   n: size of matrix
#   use_output_matrix: use the output_matrix
#
def main(n=3, use_output_matrix=0):

  model = cp.CpModel()

  #
  # data
  #
  print("n = ", n)

  # range_n = range(1, n+1)
  range_n = list(range(0, n))

  N = n * n
  range_N = list(range(1, N + 1))

  #
  # variables
  #

  # x[i,j,k] = 1 means that cell (i,j) contains integer k
  x = {}
  for i in range_n:
    for j in range_n:
      for k in range_N:
        x[i, j, k] = model.NewIntVar(0, 1, "x[%i,%i,%i]" % (i, j, k))

  # For output. 
  if use_output_matrix == 1:
    print("Using an output matrix")
    square = {}
    for i in range_n:
      for j in range_n:
        square[i, j] = model.NewIntVar(1, n * n, "square[%i,%i]" % (i, j))

  # the magic sum
  s = model.NewIntVar(1, n * n * n, "s")

  #
  # constraints
  #
  nn = ( n * (n*n + 1)) // 2
  model.Add(s == nn)

  # each cell must be assigned exactly one integer
  for i in range_n:
    for j in range_n:
      model.Add(sum([x[i, j, k] for k in range_N]) == 1)

  # each integer must be assigned exactly to one cell
  for k in range_N:
    model.Add(sum([x[i, j, k] for i in range_n for j in range_n]) == 1)

  # # the sum in each row must be the magic sum
  for i in range_n:
    model.Add(
        sum([k * x[i, j, k] for j in range_n for k in range_N]) == s)

  # # the sum in each column must be the magic sum
  for j in range_n:
    model.Add(
        sum([k * x[i, j, k] for i in range_n for k in range_N]) == s)

  # # the sum in the diagonal must be the magic sum
  model.Add(
      sum([k * x[i, i, k] for i in range_n for k in range_N]) == s)

  # # the sum in the co-diagonal must be the magic sum
  if range_n[0] == 1:
    # for range_n = 1..n
    model.Add(
        sum([k * x[i, n - i + 1, k]
                    for i in range_n
                    for k in range_N]) == s)
  else:
    # for range_n = 0..n-1
    model.Add(
        sum([k * x[i, n - i - 1, k]
                    for i in range_n
                    for k in range_N]) == s)

  # for output
  if use_output_matrix == 1:
    for i in range_n:
      for j in range_n:
        model.Add(
            square[i, j] == sum([k * x[i, j, k] for k in range_N]))

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print()

    print("s: ", solver.Value(s))
    if use_output_matrix == 1:
      for i in range_n:
        for j in range_n:
          print("%3d" % solver.Value(square[i, j]), end="")
        print()
      print()
    else:
      for i in range_n:
        for j in range_n:
          print("%3d" %
              sum([solver.Value(k * x[i, j, k]) for k in range_N]),
              "",
              end="")
        print()

    print("\nx:")
    for i in range_n:
      for j in range_n:
        for k in range_N:
          print(solver.Value(x[i, j, k]), end=" ")
        print()

  print()
  print("Walltime:", solver.WallTime())

if __name__ == "__main__":
  n = 3
  use_output_matrix = 1
  if len(sys.argv) > 1:
    n = int(sys.argv[1])

  if len(sys.argv) > 2:
    use_output_matrix = int(sys.argv[2])

  main(n, use_output_matrix)
