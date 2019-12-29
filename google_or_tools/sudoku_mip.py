# Copyright 2011 Hakan Kjellerstrand hakank@gmail.com
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

  Sudoku problem using MIP in Google or-tools.

  Based on the GLPK example sudoku.mod.

  This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  Also see my other Google CP Solver models:
      http://www.hakank.org/google_or_tools/
"""
import sys
from ortools.linear_solver import pywraplp

def main(sol = 'GLOP'):
  
  # Create the solver.

  print('Solver: ', sol)

  # GLOP
  if sol == 'GLOP':
    solver = pywraplp.Solver('GLOP',
                             pywraplp.Solver.GLOP_LINEAR_PROGRAMMING)
  # using GLPK
  if sol == 'GLPK':
    solver = pywraplp.Solver('CoinsGridGLPK',
                             pywraplp.Solver.GLPK_MIXED_INTEGER_PROGRAMMING)
  else:
  # Using CLP
      solver = pywraplp.Solver('CoinsGridCLP',
                               pywraplp.Solver.CBC_MIXED_INTEGER_PROGRAMMING)


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
        x[i,j, k] = solver.IntVar(0, 1, 'x[%i,%i,%i]' % (i, j, k))

  #
  # constraints
  #
  
  # assign pre-defined numbers using the "givens"
  for i in range_n:
    for j in range_n:
      for k in range_n:
        if givens[i-1][j-1] > X:
          if givens[i-1][j-1] == k:
            solver.Add(x[i,j,k] == 1)
          else:
            solver.Add(x[i,j,k] == 0)

  # each cell must be assigned exactly one number 
  for i in range_n:
    for j in range_n:
      solver.Add(solver.Sum([x[i,j,k] for k in range_n]) == 1)

  # cells in the same row must be assigned distinct numbers
  for i in range_n:
    for k in range_n:
      solver.Add(solver.Sum([x[i,j,k] for j in range_n]) == 1)

  # cells in the same column must be assigned distinct numbers
  for j in range_n:
    for k in range_n:
      solver.Add(solver.Sum([x[i,j,k] for i in range_n]) == 1)

  # cells in the same region must be assigned distinct numbers
  R = [1, 4, 7]
  for I in R:
    for J in R:
      for k in range_n:
        solver.Add(solver.Sum([x[i,j,k]
                               for i in range(I,I+3)
                               for j in range(J,J+3)])  == 1)


  #
  # solution and search
  #
  solver.Solve()

  print()

  print('Matrix:')
  for i in range_n:
    for j in range_n:
      for k in range_n:
        if x[i,j,k].solution_value() == 1:
          print(k,end=" ")
    print()
  print()



  print()
  print('walltime  :', solver.WallTime(), 'ms')
  if sol == 'CBC':
    print('iterations:', solver.iterations())


if __name__ == '__main__':

  sol = 'GLOP'
  if len(sys.argv) > 1:
    sol = sys.argv[1]
    if sol != 'GLPK' and sol != 'CBC' and sol != 'GLOP':
      print('Solver must be either GLOP, GLPK or CBC')
      sys.exit(1)
  
  main(sol)
