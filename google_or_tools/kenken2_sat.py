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

  KenKen puzzle in OR-tools CP-SAT Solver.

  http://en.wikipedia.org/wiki/KenKen
  '''
  KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
  several characteristics with sudoku. The name comes from Japanese and
  is translated as 'square wisdom' or 'cleverness squared'.
  ...
  The objective is to fill the grid in with the digits 1 through 6 such that:

    * Each row contains exactly one of each digit
    * Each column contains exactly one of each digit
    * Each bold-outlined group of cells is a cage containing digits which
      achieve the specified result using the specified mathematical operation:
        addition (+),
        subtraction (-),
        multiplication (x),
        and division (/).
        (Unlike in Killer sudoku, digits may repeat within a group.)

  ...
  More complex KenKen problems are formed using the principles described
  above but omitting the symbols +, -, x and /, thus leaving them as
  yet another unknown to be determined.
  '''


  The solution is:

    5 6 3 4 1 2
    6 1 4 5 2 3
    4 5 2 3 6 1
    3 4 1 2 5 6
    2 3 6 1 4 5
    1 2 5 6 3 4


  This is a port of my old CP model kenken2.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *
from functools import reduce



def prod_b(model, x, p, b):
  """
  prod(model, x, p, b)

  `p` is the product of the elements in array `x` if b == true
  p = x[0]*x[1]*...x[-1]  <=> b

  Note: This trickery is needed since `AddMultiplicationEquality`
        (as of writing) don't support more than two products at a time.
  
  Cf: prod(model, x, p) in cp_sat_utils.py
  """
  n = len(x)
  lb, ub = x[0].Proto().domain
  t = [model.NewIntVar(lb,ub**(i+1),"t") for i in range(n)]
  model.Add(t[0] == x[0])
  for i in range(1,n):
    model.AddMultiplicationEquality(t[i],[t[i-1],x[i]])
  model.Add(p == t[-1]).OnlyEnforceIf(b)
  model.Add(p != t[-1]).OnlyEnforceIf(b.Not())


#
# Ensure that the sum of the segments
# in cc == res
#
def calc(model,cc, x, res):

  if len(cc) == 2:

    # for two operands there may be
    # a lot of variants: +, -, *, /
    c00, c01 = cc[0]
    c10, c11 = cc[1]
    a = x[c00 - 1, c01 - 1]
    b = x[c10 - 1, c11 - 1]

    r = [model.NewBoolVar(f"r{i}") for i in range(6)]

    # Addition
    model.Add(a + b == res).OnlyEnforceIf(r[0])
    model.Add(a + b != res).OnlyEnforceIf(r[0].Not())
    
    # Multiplication / Division

    #
    # Note: What I can tell, one cannot use OnlyEnforceIf together 
    # with AddMultiplicationEquality, e.g. this don't work:
    #    model.AddMultiplicationEquality(a,[b,res]).OnlyEnforceIf(r[2])
    #

    max_prod = 8*9

    # a*b = res
    # model.AddMultiplicationEquality(res,[a,b]).OnlyEnforceIf(r[1]) # don't work
    t1 = model.NewIntVar(0,max_prod,"t1")
    model.AddMultiplicationEquality(t1,[a,b])
    model.Add(t1 == res).OnlyEnforceIf(r[1])
    model.Add(t1 != res).OnlyEnforceIf(r[1].Not())

    # b/a == res  -> b*res = a
    t2 = model.NewIntVar(0,max_prod,"t2")
    model.AddDivisionEquality(t2,a,b)
    model.Add(t2 == res).OnlyEnforceIf(r[2])
    model.Add(t2 != res).OnlyEnforceIf(r[2].Not())


    # a/b == res  -> a*res = b
    t3 = model.NewIntVar(0,max_prod,"t3")
    model.AddDivisionEquality(t3,b,a)
    model.Add(t3 == res).OnlyEnforceIf(r[3])
    model.Add(t3 != res).OnlyEnforceIf(r[3].Not())

    # Subtraction
    model.Add(a - b == res).OnlyEnforceIf(r[4])
    model.Add(a - b != res).OnlyEnforceIf(r[4].Not())

    model.Add(b - a == res).OnlyEnforceIf(r[5])
    model.Add(b - a != res).OnlyEnforceIf(r[5].Not())

    # model.Add(sum(r) >= 1)
    model.AddBoolOr(r)

  else:

    # res is either sum or product of the segment

    xx = [x[i[0] - 1, i[1] - 1] for i in cc]

    # Sum
    this_sum_b = model.NewBoolVar("this_sum")
    model.Add(res == sum(xx)).OnlyEnforceIf(this_sum_b)

    # Product
    this_prod_b = model.NewBoolVar("this_prod_b")
    prod_b(model, xx,res, this_prod_b)
    model.Add(this_sum_b + this_prod_b >= 1)


def main():

  model = cp.CpModel()

  #
  # data
  #

  # size of matrix
  n = 6

  # For a better view of the problem, see
  #  http://en.wikipedia.org/wiki/File:KenKenProblem.svg

  # hints
  #    [sum, [segments]]
  # Note: 1-based
  problem = [[11, [[1, 1], [2, 1]]], [2, [[1, 2], [1, 3]]],
             [20, [[1, 4], [2, 4]]], [6, [[1, 5], [1, 6], [2, 6], [3, 6]]],
             [3, [[2, 2], [2, 3]]], [3, [[2, 5], [3, 5]]],
             [240, [[3, 1], [3, 2], [4, 1], [4, 2]]], [6, [[3, 3], [3, 4]]],
             [6, [[4, 3], [5, 3]]], [7, [[4, 4], [5, 4], [5, 5]]],
             [30, [[4, 5], [4, 6]]], [6, [[5, 1], [5, 2]]],
             [9, [[5, 6], [6, 6]]], [8, [[6, 1], [6, 2], [6, 3]]],
             [2, [[6, 4], [6, 5]]]]

  #
  # variables
  #

  # the set
  x = {}
  for i in range(n):
    for j in range(n):
      x[i, j] = model.NewIntVar(1, n, "x[%i,%i]" % (i, j))

  #
  # constraints
  #

  # all rows and columns must be unique
  for i in range(n):
    row = [x[i, j] for j in range(n)]
    model.AddAllDifferent(row)

    col = [x[j, i] for j in range(n)]
    model.AddAllDifferent(col)

  # calculate the segments
  for (res, segment) in problem:
    calc(model, segment, x, res)

  #
  # search and solution
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    for i in range(n):
      for j in range(n):
        print(solver.Value(x[i, j]), end=" ")
      print()

    print()


  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
