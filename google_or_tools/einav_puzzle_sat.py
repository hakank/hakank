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

  A programming puzzle from Einav in OR-tools CP-SAT Solver.

  From
  'A programming puzzle from Einav'
  http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
  '''
  My friend Einav gave me this programming puzzle to work on. Given
  this array of positive and negative numbers:
  33   30  -10 -6  18   7  -11 -23   6
  ...
  -25   4  16  30  33 -23  -4   4 -23

  You can flip the sign of entire rows and columns, as many of them
  as you like. The goal is to make all the rows and columns sum to positive
  numbers (or zero), and then to find the solution (there are more than one)
  that has the smallest overall sum. So for example, for this array:
  33  30 -10
  -16  19   9
  -17 -12 -14
  You could flip the sign for the bottom row to get this array:
  33  30 -10
  -16  19   9
  17  12  14
  Now all the rows and columns have positive sums, and the overall total is
  108.
  But you could instead flip the second and third columns, and the second
  row, to get this array:
  33  -30  10
  16   19    9
  -17   12   14
  All the rows and columns still total positive, and the overall sum is just
  66. So this solution is better (I don't know if it's the best)
  A pure brute force solution would have to try over 30 billion solutions.
  I wrote code to solve this in J. I'll post that separately.
  '''

  This is a port of my old CP model einav_puzzle.py

  After a bit of tweaking with the exclusion of 0 in the domain
  of row_signs and col_signs this model now work. See comments below.

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

  # small problem
  # rows = 3;
  # cols = 3;
  # data = [
  #     [ 33,  30, -10],
  #     [-16,  19,   9],
  #     [-17, -12, -14]
  #     ]

  # Full problem
  rows = 27
  cols = 9
  data = [[33, 30, 10, -6, 18, -7, -11, 23, -6],
          [16, -19, 9, -26, -8, -19, -8, -21, -14],
          [17, 12, -14, 31, -30, 13, -13, 19, 16],
          [-6, -11, 1, 17, -12, -4, -7, 14, -21],
          [18, -31, 34, -22, 17, -19, 20, 24, 6],
          [33, -18, 17, -15, 31, -5, 3, 27, -3],
          [-18, -20, -18, 31, 6, 4, -2, -12, 24],
          [27, 14, 4, -29, -3, 5, -29, 8, -12],
          [-15, -7, -23, 23, -9, -8, 6, 8, -12],
          [33, -23, -19, -4, -8, -7, 11, -12, 31],
          [-20, 19, -15, -30, 11, 32, 7, 14, -5],
          [-23, 18, -32, -2, -31, -7, 8, 24, 16],
          [32, -4, -10, -14, -6, -1, 0, 23, 23],
          [25, 0, -23, 22, 12, 28, -27, 15, 4],
          [-30, -13, -16, -3, -3, -32, -3, 27, -31],
          [22, 1, 26, 4, -2, -13, 26, 17, 14],
          [-9, -18, 3, -20, -27, -32, -11, 27, 13],
          [-17, 33, -7, 19, -32, 13, -31, -2, -24],
          [-31, 27, -31, -29, 15, 2, 29, -15, 33],
          [-18, -23, 15, 28, 0, 30, -4, 12, -32],
          [-3, 34, 27, -25, -18, 26, 1, 34, 26],
          [-21, -31, -10, -13, -30, -17, -12, -26, 31],
          [23, -31, -19, 21, -17, -10, 2, -23, 23],
          [-3, 6, 0, -3, -32, 0, -10, -25, 14],
          [-19, 9, 14, -27, 20, 15, -5, -27, 18],
          [11, -6, 24, 7, -17, 26, 20, -31, -25],
          [-25, 4, -16, 30, 33, 23, -4, -4, 23]]

  #
  # variables
  #
  x = {} # row_signs[i] x col_signt[j] x data[i,j]
  x_flat = []
  for i in range(rows):
    for j in range(cols):
      x[i, j] = model.NewIntVar(-100, 100, 'x[%i,%i]' % (i, j))
      x_flat.append(x[i,j])

  row_sums = [model.NewIntVar(0, 300, 'row_sums(%i)' % i) for i in range(rows)]
  col_sums = [model.NewIntVar(0, 300, 'col_sums(%i)' % j) for j in range(cols)]


  # This don't work!
  # dom = cp.Domain.FromValues([-1,1])
  # row_signs = [model.NewIntVarFromDomain(dom, 'row_signs(%i)' % i) for i in range(rows)]
  # col_signs = [model.NewIntVarFromDomain(dom, 'col_signs(%i)' % j) for j in range(cols)]
  # Instead we give the domain -1..1 and restrict 0 below
  row_signs = [model.NewIntVar(-1,1,'row_signs(%i)' % i) for i in range(rows)]
  col_signs = [model.NewIntVar(-1,1, 'col_signs(%i)' % j) for j in range(cols)]

  # total sum: to be minimized
  total_sum = model.NewIntVar(0, 1000, 'total_sum')

  #
  # constraints
  #
  # print("x:", x)
  for i in range(rows):
    for j in range(cols):
      # model.Add(x[i, j] == data[i][j] * row_signs[i] * col_signs[j])
      t = model.NewIntVar(-100,100,"t")
      # Note: it seems that it's this constraint that makes
      # it INFEASSIBLE! Without it there's a solution, but it 
      # - of course - wrong.
      # In Aug 2020 there was a report about multiplication of 
      # negative numbers:
      # https://github.com/google/or-tools/issues/2133
      # Ah, and in Sep 18 2020
      # https://groups.google.com/g/or-tools-discuss/c/Gbp4wp6AJH8/m/779gmhhJAQAJ
      # Laurent comments:
      # """
      # We currently do not support multiplications of integer spanning across 0. 
      # I have never found a real application for this. 
      # Why do you need it?
      # """"
      # Later: I checked using normal domain -1..1 and restrict 0 later (below)
      # but that also yielded INFEASIBLE
      # A little later: And now this model work!
      model.AddMultiplicationEquality(t, [row_signs[i],col_signs[j]])
      model.AddMultiplicationEquality(x[i,j], [t,data[i][j]])

  model.Add(total_sum == sum(x_flat))

  

  # row sums
  for i in range(rows):
    # Exclude 0 from row_signs
    # model.Add(row_signs[i] != 0) # -> INFEASIBLE
    
    # This works!
    b1 = model.NewBoolVar("row_sign[%i]_b1" % i)
    b2 = model.NewBoolVar("row_sign[%i]_b2" % i)
    model.Add(row_signs[i] < 0).OnlyEnforceIf(b1)
    model.Add(row_signs[i] > 0).OnlyEnforceIf(b2)
    model.Add(b1 + b2 >= 1)
    
    model.Add(row_sums[i] == sum([x[i,j] for j in range(cols)]))

  # column sums
  for j in range(cols):
    # model.Add(col_signs[j] != 0) # -> INFEASIBLE
    b1 = model.NewBoolVar("col_sign[%i]_b1" % j)
    b2 = model.NewBoolVar("col_sign[%i]_b2" % j)
    model.Add(col_signs[j] < 0).OnlyEnforceIf(b1)
    model.Add(col_signs[j] > 0).OnlyEnforceIf(b2)
    model.Add(b1 + b2 >= 1)
    
    model.Add(col_sums[j] == sum([x[i,j] for i in range(rows)]))

  # objective
  model.Minimize(total_sum)

  #
  # search and result
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  status = solver.Solve(model)
  print("status:", solver.StatusName(status))
  
  if status == cp.OPTIMAL:
    print('total_sum:', solver.Value(total_sum))
    print('row_sums:', [solver.Value(row_sums[i]) for i in range(rows)])
    print('col_sums:', [solver.Value(col_sums[j]) for j in range(cols)])
    print('row_signs:', [solver.Value(row_signs[i]) for i in range(rows)])
    print('col_signs:', [solver.Value(col_signs[j]) for j in range(cols)])
    print('x:')
    for i in range(rows):
      for j in range(cols):
        print('%3i' % solver.Value(x[i, j]), end=' ')
      print()
    print()
  else:
    print("No solution!")


  print()
  # print('num_solutions:', num_solutions)
  print('NumConflicts:', solver.NumConflicts())
  print('NunBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime(), 'ms')


if __name__ == '__main__':
  main()
