"""
Bales of hay problem in cpmpy.

From The Math Less Traveled, 
'The haybaler', http://www.mathlesstraveled.com/?p=582 
'''
You have five bales of hay.

For some reason, instead of being weighed individually, they were weighed 
in all possible combinations of two. The weights of each of these 
combinations were written down and arranged in numerical order, without 
keeping track of which weight matched which pair of bales. The weights, 
in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.

How much does each bale weigh? Is there a solution? Are there multiple 
possible solutions? 
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def bales_of_hay():

  n = 5
  weights =  [80, 82, 83, 84, 85, 86, 87, 88, 90, 91]

  # variables
  bales = intvar(0,50, shape=n,name="bales")

  model = Model()

  # constraints
  model += [increasing(bales)]
  
  for w in weights:
    i = intvar(0,n-1)
    j = intvar(0,n-1)
    model += [i < j]
    model += [w == bales[i] + bales[j]]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("bales:",bales.value())
  print()

  print('num_solutions:', num_solutions)


bales_of_hay()
