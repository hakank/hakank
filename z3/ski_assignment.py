#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Ski assignment in Z3
#
# From   Jeffrey Lee Hellrung, Jr.:
# PIC 60, Fall 2008 Final Review, December 12, 2008
# http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
# """
# 5. Ski Optimization! Your job at Snapple is pleasant but in the winter
# you've decided to become a ski bum. You've hooked up with the Mount
# Baldy Ski Resort. They'll let you ski all winter for free in exchange
# for helping their ski rental shop with an algorithm to assign skis to
# skiers. Ideally, each skier should obtain a pair of skis whose height
# matches his or her own height exactly. Unfortunately, this is generally
# not possible. We define the disparity between a skier and his or her
# skis to be the absolute value of the difference between the height of
# the skier and the pair of skis. Our objective is to find an assignment
# of skis to skiers that minimizes the sum of the disparities.
# ...
# Illustrate your algorithm by explicitly filling out the A[i, j] table
# for the following sample data:
#   * Ski heights: 1, 2, 5, 7, 13, 21.
#   * Skier heights: 3, 4, 7, 11, 18.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

def convert_to_array(sol,x,name):
  A = Array(name,IntSort(),IntSort())
  for i in range(len(x)):
    sol.add(A[i] == x[i])
  return A

def ski_assignment():
  sol = Optimize()

  # data
  num_skis = 6
  num_skiers = 5
  ski_heights = [1, 2, 5, 7, 13, 21]
  ski_heights_a = convert_to_array(sol,ski_heights,"ski_heights")
  
  skier_heights = [3, 4, 7, 11, 18]
  skier_heights_a = convert_to_array(sol,skier_heights,"skier_heights")

  # variables

  # which ski to choose for each skier
  x = Array("x", IntSort(), IntSort())
  for i in range(num_skiers):
      sol.add(x[i] >= 0, x[i] <= num_skis-1)
  z = Int('z')
  sol.add(z >= 0, z <= sum(ski_heights))

  # constraints
  sol.add(Distinct([x[i] for i in range(num_skiers)]))

  # Note: abs is defined in z3_hakank_utils.py
  sol.add(z == Sum([Abs(ski_heights_a[x[i]] - skier_heights_a[i]) for i in range(num_skiers)] ))

  # objective
  sol.minimize(z)

  # search and result
  if sol.check() == sat:
    mod = sol.model()
    print("total differences:", mod.eval(z))
    for i in range(num_skiers):
      x_val = mod.eval(x[i])
      ski_height = mod.eval(ski_heights_a[x_val])
      diff = mod.eval(ski_height - mod.eval(skier_heights_a[i]))
      print("Skier %i: Ski %s with length %2s (diff: %2s)" % (i, x_val, ski_height, diff))

    print()


ski_assignment()
