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

  Sudoku problem (MIP approach) in OR-tools CP-SAT solver.

  Based on the GLPK example sudoku.mod.

  This is a port of my old LP model sudoku_mip.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

def main():

  model = cp.CpModel()  

  #
  # data
  #
  n = 9
  range_n = range(1, n + 1)
  
  X = -1
  # Example from Wikipedia
  givens = [[5, 3, X, X, 7, X, X, X, X],
            [6, X, X, 1, 9, 5, X, X, X],
            [X, 9, 8, X, X, X, X, 6, X],
            [8, X, X, X, 6, X, X, X, 3],
            [4, X, X, 8, X, 3, X, X, 1],
            [7, X, X, X, 2, X, X, X, 6],
            [X, 6, X, X, X, X, 2, 8, X],
            [X, X, X, 4, 1, 9, X, X, 5],
            [X, X, X, X, 8, X, X, 7, 9]]

  #
  # variables
  #

  # x[i,j,k] = 1 means cell [i,j] is assigned number k
  x = {}
  for i in range_n:
    for j in range_n:
      for k in range_n:      
        x[i,j, k] = model.NewIntVar(0, 1, 'x[%i,%i,%i]' % (i, j, k))

  #
  # constraints
  #
  
  # assign pre-defined numbers using the "givens"
  for i in range_n:
    for j in range_n:
      for k in range_n:
        if givens[i-1][j-1] > X:
          if givens[i-1][j-1] == k:
            model.Add(x[i,j,k] == 1)
          else:
            model.Add(x[i,j,k] == 0)

  # each cell must be assigned exactly one number 
  for i in range_n:
    for j in range_n:
      model.Add(sum([x[i,j,k] for k in range_n]) == 1)

  # cells in the same row must be assigned distinct numbers
  for i in range_n:
    for k in range_n:
      model.Add(sum([x[i,j,k] for j in range_n]) == 1)

  # cells in the same column must be assigned distinct numbers
  for j in range_n:
    for k in range_n:
      model.Add(sum([x[i,j,k] for i in range_n]) == 1)

  # cells in the same region must be assigned distinct numbers
  R = [1, 4, 7]
  for I in R:
    for J in R:
      for k in range_n:
        model.Add(sum([x[i,j,k]
                               for i in range(I,I+3)
                               for j in range(J,J+3)])  == 1)


  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('Matrix:')
    for i in range_n:
      for j in range_n:
        for k in range_n:
          if solver.Value(x[i,j,k]) == 1:
            print(k,end=" ")
      print()
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == '__main__':
  main()
