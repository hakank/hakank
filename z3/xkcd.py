#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# xkcd problem in Z3
#
# See http://xkcd.com/287/
# 
# Some amount (or none) of each dish should be ordered to give a total
# of exact 15.05

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

def xkcd():

  # Create the solver.
  sol = Solver()

  #
  # data
  #
  num_prices = 6
  # for price and total: multiplied by 100 to be able to use integers
  price = [215, 275, 335, 355, 420, 580]
  total = 1505

  products = ["mixed fruit", "french fries", "side salad",
              "host wings", "mozzarella sticks", "samples place"]

  # declare variables

  # how many items of each dish
  x = [Int("x%i" % i) for i in range(num_prices)]
  for i in range(num_prices):
      sol.add(x[i] >= 0, x[i]<=10)

  # constraints
  sol.add(total == Sum([x[i] * price[i] for i in range(num_prices)]))

  num_solutions = 0
  while sol.check() == sat:
      num_solutions += 1
      mod = sol.model()
      xval = [mod.eval(x[i]) for i in range(num_prices)]
      print "x:", xval
      for i in range(num_prices):
        if xval[i].as_long() > 0:
          print xval[i], "of", products[i], ":", price[i] / 100.0
      print
      sol.add(Or([x[i] != xval[i] for i in range(num_prices)]))

  print "num_solutions:", num_solutions

if __name__ == "__main__":
  xkcd()


