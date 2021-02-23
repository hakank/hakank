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

  Ski assignment in OR-tools CP-SAT Solver.

  From   Jeffrey Lee Hellrung, Jr.:
  PIC 60, Fall 2008 Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  '''
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter
  you've decided to become a ski bum. You've hooked up with the Mount
  Baldy Ski Resort. They'll let you ski all winter for free in exchange
  for helping their ski rental shop with an algorithm to assign skis to
  skiers. Ideally, each skier should obtain a pair of skis whose height
  matches his or her own height exactly. Unfortunately, this is generally
  not possible. We define the disparity between a skier and his or her
  skis to be the absolute value of the difference between the height of
  the skier and the pair of skis. Our objective is to find an assignment
  of skis to skiers that minimizes the sum of the disparities.
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table
  for the following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.
  '''

  This is a port of my old CP model ski_assignment.py

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
  num_skis = 6
  num_skiers = 5
  ski_heights = [1, 2, 5, 7, 13, 21]
  skier_heights = [3, 4, 7, 11, 18]

  #
  # variables
  #

  # which ski to choose for each skier
  x = [model.NewIntVar(0, num_skis - 1, 'x[%i]' % i) for i in range(num_skiers)]
  z = model.NewIntVar(0, sum(ski_heights), 'z')

  #
  # constraints
  #
  model.AddAllDifferent(x)

  # Old:
  # z_tmp = [
  #     abs(solver.Element(ski_heights, x[i]) - skier_heights[i])
  #     for i in range(num_skiers)
  # ]
  z_tmp = [model.NewIntVar(0,100,"z_tmp") for i in range(num_skiers)]
  for i in range(num_skiers):
    t1 = model.NewIntVar(-100,100, "t1")
    model.AddElement(x[i], ski_heights, t1)
    t2 = model.NewIntVar(-100,100, "t1")
    model.Add(t2 == t1 - skier_heights[i])
    model.AddAbsEquality(z_tmp[i], t2)
  model.Add(z == sum(z_tmp))

  # objective
  model.Minimize(z)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('total differences:', solver.Value(z))
    for i in range(num_skiers):
      x_val = solver.Value(x[i])
      ski_height = ski_heights[solver.Value(x[i])]
      diff = ski_height - skier_heights[i]
      print('Skier %i: Ski %i with length %2i (diff: %2i)' %\
            (i, x_val, ski_height, diff))
    print()

  print()
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
