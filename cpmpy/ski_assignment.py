"""
Ski assignment in cpmpy

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
  
This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def ski_assignment():

  # data
  num_skis = 6
  num_skiers = 5
  ski_heights = [1, 2, 5, 7, 13, 21]
  skier_heights = [3, 4, 7, 11, 18]

  # which ski to choose for each skier
  x = intvar(0,num_skis-1,shape=num_skiers,name="x")
  z = intvar(0, sum(ski_heights), name="z")


  model = Model(minimize=z)
  
  # constraints
  model += [AllDifferent(x)]

  # model += [z == sum([abs(ski_heights[x[i]] - skier_heights[i]) for i in range(num_skiers)] )]
  model += [z == sum([abs(Element(ski_heights,x[i]) - skier_heights[i]) for i in range(num_skiers)] )]
  
  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("total differences:", z.value())
    for i in range(num_skiers):
      x_val = x[i].value()
      ski_height = ski_heights[x[i].value()]
      diff = ski_height - skier_heights[i]
      print('Skier %i: Ski %i with length %2i (diff: %2i)' %\
            (i, x_val, ski_height, diff))
    print()

  print()
  print('num_solutions:', num_solutions)

  return ss


ss = ski_assignment()

