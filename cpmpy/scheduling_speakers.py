"""
Scheduling speakers problem in cpmpy.

From Rina Dechter, Constraint Processing, page 72
Scheduling of 6 speakers in 6 slots.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *




def scheduling_speakers():

  model = Model()
  
  # data
  n = 6  # number of speakers

  # slots available to speak
  available = [
      # Reasoning:
      [3, 4, 5, 6],  # 2) the only one with 6 after speaker F -> 1
      [3, 4],  # 5) 3 or 4
      [2, 3, 4, 5],  # 3) only with 5 after F -> 1 and A -> 6
      [2, 3, 4],  # 4) only with 2 after C -> 5 and F -> 1
      [3, 4],  # 5) 3 or 4
      [1, 2, 3, 4, 5, 6]  # 1) the only with 1
  ]

  #
  # variables
  #
  x = intvar(1,n,shape=n,name="x")

  #
  # constraints
  #
  model += [AllDifferent(x)]

  for i in range(n):
    model += [member_of(available[i],x[i])]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print('num_solutions:', num_solutions)


scheduling_speakers()
