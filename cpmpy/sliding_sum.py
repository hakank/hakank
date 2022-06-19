"""
Sliding sum constraint in cpmpy.

From Global Constraint Catalogue
http://www.emn.fr/x-info/sdemasse/gccat/Csliding_sum.html
'''
sliding_sum(LOW,UP,SEQ,VARIABLES)

Purpose

Constrains all sequences of SEQ consecutive variables of the collection VARIABLES so that the 
sum of the variables belongs to interval [LOW, UP].

Example
    (
    3, 7, 4,<1, 4, 2, 0, 0, 3, 4>
    )

The example considers all sliding sequences of SEQ=4 consecutive values of <1, 4, 2, 0,0,3, 4> 
collection and constraints the sum to be in [LOW,UP] = [3, 7]. The sliding_sum constraint holds 
since the sum associated with the corresponding subsequences 1 4 2 0, 4 2 0 0, 2 0 0 3, and 
0 0 3 4 are respectively 7, 6, 5 and 7. 
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def sliding_sum_test(n=7,seq=4,low=3,up=7):

  x = intvar(0,4,shape=n,name="x")
  # low = intvar(0,10,name="low")
  # up = intvar(0,10,name="up")
  
  model = Model(sliding_sum(low,up,seq,x))

  ss = CPM_ortools(model)
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:", num_solutions)


sliding_sum_test()
