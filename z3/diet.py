#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Simple diet problem in in Z3
#
# Standard Operations Research example.
#
# Minimize the cost for the products:
# Type of                        Calories   Chocolate    Sugar    Fat
# Food                                      (ounces)     (ounces) (ounces)
# Chocolate Cake (1 slice)       400           3            2      2
# Chocolate ice cream (1 scoop)  200           2            2      4
# Cola (1 bottle)                150           0            4      1
# Pineapple cheesecake (1 piece) 500           0            4      5
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *


#
# Diet problem:
#  variant = 1: use direct sums
#  variant = 2: use scalar product
#
def diet(variant=1):

  sol = Optimize()

  # data
  n = 4
  price = [50, 20, 30, 80]  # in cents
  limits = [500, 6, 10, 8]  # requirements for each nutrition type

  # nutritions for each product
  calories  = [400, 200, 150, 500]
  chocolate = [  3,   2,   0,   0]
  sugar     = [  2,   2,   4,   4]
  fat       = [  2,   4,   1,   5]

  #
  # declare variables
  #
  x = makeIntVars(sol, "x",n,0,100)
  cost = makeIntVar(sol,"cost",0, 10000)

  #
  # constraints
  #
  if variant == 1:
      sol.add(Sum([x[i] * calories[i] for i in range(n)]) >= limits[0])
      sol.add(Sum([x[i] * chocolate[i] for i in range(n)]) >= limits[1])
      sol.add(Sum([x[i] * sugar[i] for i in range(n)]) >= limits[2])
      sol.add(Sum([x[i] * fat[i] for i in range(n)]) >= limits[3])
  else:
      sol.add(scalar_product2(sol,x, calories) >= limits[0])
      sol.add(scalar_product2(sol,x, chocolate) >= limits[1])
      sol.add(scalar_product2(sol,x, sugar) >= limits[2])
      sol.add(scalar_product2(sol,x, fat) >= limits[3])
      
  # objective
  sol.minimize(cost)

  # solution

  if sol.check() == sat:
      mod = sol.model()
      print("cost:", mod.eval(cost))
      print([("abcdefghij"[i], mod.eval(x[i])) for i in range(n)])


if __name__ == "__main__":
  diet(1)
  print()
  diet(2)
