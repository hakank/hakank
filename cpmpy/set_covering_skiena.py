"""
Set covering in cpmpy.

Example from Steven Skiena, The Stony Brook Algorithm Repository
http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
'''
Input Description: A set of subsets S_1, ..., S_m of the
universal set U = {1,...,n}.

Problem: What is the smallest subset of subsets T subset S such
that \cup_{t_i in T} t_i = U?
'''
Data is from the pictures INPUT/OUTPUT.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def set_covering_skiena():

  #
  # data
  #
  num_sets = 7
  num_elements = 12
  belongs = [
      # 1 2 3 4 5 6 7 8 9 0 1 2  elements
      [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],  # Set 1
      [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],  # 2
      [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0],  # 3
      [0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0],  # 4
      [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],  # 5
      [1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0],  # 6
      [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1]  # 7
  ]

  #
  # variables
  #
  x = boolvar(shape=num_sets,name="x")

  # number of choosen sets
  z = intvar(0, num_sets * 2, name="z")

  # total number of elements in the choosen sets
  tot_elements = intvar(0, num_sets*num_elements,name="tot_elements")

  model = Model(minimize=z)

  #
  # constraints
  #
  model += [z == sum(x)]

  # all sets must be used
  for j in range(num_elements):
    s = sum([belongs[i][j] * x[i] for i in range(num_sets)])
    model += [s >= 1]


  # number of used elements
  model += [tot_elements == sum([
      x[i] * belongs[i][j] for i in range(num_sets) for j in range(num_elements)
  ])]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("z", z.value())
    print("tot_elements:", tot_elements.value())
    print("x:", x.value())

  print()
  print('num_solutions:', num_solutions)


set_covering_skiena()
