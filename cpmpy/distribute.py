"""
Global constraint distribute in cpmpy.

Decomposition of global constraint distribute.

From MiniZinc:
'''
Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
'''

The simple example
  card_val  = [4, None, 1, None]
  value_val = [None, 7, 8, None]
  base_val  = [None, 7, 6, 8, 6, 9, None]

Give this unique solution:
  card : [4 1 1 1]
  value: [6 7 8 9]
  base : [6 7 6 8 6 9 6]

- For value 6 there are 4 occurrences in base
- For value 7 there is 1 occurrece in base
- For value 8 there is 1 occurrece in base
- For value 8 there is 1 occurrece in base


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def distribute_test():

  model = Model()

  n = 4
  card = intvar(1,10,shape=n,name="card")
  value = intvar(1,10,shape=n,name="value")
  base = intvar(1,10,shape=7,name="base")
  
  # test values
  card_val  = [4, None, 1, None]
  value_val = [None, 7, 8, None]
  # base_val  = [None, 7, 6, 8, 6, 9, None] # Original
  # This is a little more interesting  
  base_val  = [None, 7, 6, 8, 6, None, None] 

  model += (fill_array(card,card_val))
  model += (fill_array(value,value_val))
  model += (fill_array(base,base_val))

  model += (distribute(card, value, base))
   
  def print_sol():
    print("card :", card.value())
    print("value:", value.value())
    print("base :", base.value())
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

distribute_test()
