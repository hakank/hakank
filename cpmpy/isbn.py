"""
Some explorations of ISBN13 in cpmpy.

See http://en.wikipedia.org/wiki/ISBN

Test ISBN:
978-0262720304: The OPL Optimization Programming Language 
[9,7,8,0,2,6,2,7,2,0,3,0,4]

isbn = 978-0262220774: Constraint-based Local Search
[9,7,8,0,2,6,2,2,2,0,7,7,4];

Constraint Solving and Planning with Picat
http://www.springer.com/gp/book/9783319258812
Paperback: [9,7,8,3,3,1,9,2,5,8,8,1,2]
Ebook    : [9,7,8,3,3,1,9,2,5,8,8,3,6]

This is a fairly general model which can - rather easily -
be adjusted to similar algorithms.
This is left as an exercise to the reader. :-)


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def isbn(isbn_init):
  print("isbn_init:", isbn_init)
  
  model = Model()
  
  n = 13

  # variables
  isbn = intvar(0,9,shape=n,name="isbn")
  # multipliers
  mult0 = intvar(1,9,name="mult0")
  mult1 = intvar(1,9,name="mult1")
  
  # The first n-1 digits, for the check sum
  tt = intvar(0,1000,shape=n-1,name="tt")
  # sum of tt
  tsum = intvar(0,1000,name="tsum")
  
  for i in range(n):
      if isbn_init[i] >= 0:
          model += [isbn[i] == isbn_init[i]]

  # These are the multiples for ISBN
  model += [mult0 == 3]
  model += [mult1 == 1]

  #  model(mult0 != mult1) # extra constraint

  # isbn13 starts with 978 or 979
  model += [isbn[0] == 9]
  model += [isbn[1] == 7]
  model += [isbn[2] >= 8]

  # Prepare for the check sum
  for i in range(n-1): 
      if (i+1) % 2 == 0:
          model += [tt[i] == isbn[i]*mult0]
      else:
          model += [tt[i] == isbn[i]*mult1]

  model += [tsum == sum(tt)]

  # check digit
  model += [isbn[-1] == ((10 - (tsum % 10)) % 10)]

  # print(model)

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=isbn)
  print("num_solutions:", num_solutions)
  print()

# Let's test with our Picat book
# [9,7,8,3,3,1,9,2,5,8,8,1,2]

X = -1 # The unknown
isbn_test = [9,7,8,3,3,1,9,2,5,8,8,1,X]
isbn(isbn_test)

isbn_test = [9,7,8,3,3,1,9,2,5,8,8,X,2]
isbn(isbn_test)


isbn_test = [9,7,8,3,3,1,9,2,5,8,8,X,X]
isbn(isbn_test)
