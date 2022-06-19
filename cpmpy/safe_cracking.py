"""
Safe cracking puzzle in cpmpy.

From the Oz Primer:
http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
'''
The code of Professor Smart's safe is a sequence of 9 distinct
nonzero digits C1 .. C9 such that the following equations and
inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

and

   C1 <> 1, C2 <> 2, ..., C9 <> 9

can you find the correct combination?
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def safe_cracking():

  model = Model()

  # data
  n = 9

  # variables
  LD = intvar(1,n,shape=n,name="LD")
  C1, C2, C3, C4, C5, C6, C7, C8, C9 = LD

  #
  # constraints
  #
  model += [AllDifferent(LD)]

  model += [C4 - C6 == C7]
  model += [C1 * C2 * C3 == C8 + C9]
  model += [C2 + C3 + C6 < C8]
  model += [C9 < C8]
  for i in range(n):
    model += [LD[i] != i + 1]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=LD)
  print()
  print('num_solutions:', num_solutions)



safe_cracking()
