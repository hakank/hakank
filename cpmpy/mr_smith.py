"""
Mr Smith problem in cpmpy

From an IF Prolog example (http://www.ifcomputer.de/)
'''
The Smith family and their three children want to pay a visit but they
do not all have the time to do so. Following are few hints who will go
and who will not:
  o If Mr Smith comes, his wife will come too.
  o At least one of their two sons Matt and John will come.
  o Either Mrs Smith or Tim will come, but not both.
  o Either Tim and John will come, or neither will come.
  o If Matt comes, then John and his father will
    also come.
'''

The answer should be:
  Mr_Smith_comes      =  0
  Mrs_Smith_comes     =  0
  Matt_comes          =  0
  John_comes          =  1
  Tim_comes           =  1

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



#
# This model use the "Linear Programming approach".
#
def mr_smith_v1():

  model = Model()

  # data
  n = 5

  # declare variables
  x = boolvar(shape=n)
  Mr_Smith, Mrs_Smith, Matt, John, Tim = x

  #
  # I've kept the MiniZinc constraints for clarity
  # and debugging.
  #

  # If Mr Smith comes then his wife will come too.
  # (Mr_Smith -> Mrs_Smith)
  model += [Mr_Smith - Mrs_Smith <= 0]

  # At least one of their two sons Matt and John will come.
  # (Matt \/ John)
  model += [Matt + John >= 1]

  # Either Mrs Smith or Tim will come but not both.
  # bool2int(Mrs_Smith) + bool2int(Tim) = 1 /\
  # (Mrs_Smith xor Tim)
  model += [Mrs_Smith + Tim == 1]

  # Either Tim and John will come or neither will come.
  # (Tim = John)
  model += [Tim == John]

  # If Matt comes /\ then John and his father will also come.
  # (Matt -> (John /\ Mr_Smith))
  model += [Matt - (John * Mr_Smith) <= 0]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print('num_solutions:', num_solutions)

#
# This version use logical operators:
#  - or: |
#  - and: &
#  - xor: ^
#  - implies: implies (it's a pity that '->' or '=>' is not supported
#  - equivalence: ==
#
def mr_smith_v2():

  model = Model()

  # data
  n = 5

  # declare variables
  x = boolvar(shape=n)
  Mr_Smith, Mrs_Smith, Matt, John, Tim = x

  #
  # I've kept the MiniZinc constraints for clarity
  # and debugging.
  #

  # If Mr Smith comes then his wife will come too.
  # (Mr_Smith -> Mrs_Smith)
  model += [Mr_Smith.implies(Mrs_Smith)]

  # At least one of their two sons Matt and John will come.
  # (Matt \/ John)
  model += [Matt | John]

  # Either Mrs Smith or Tim will come but not both.
  # (Mrs_Smith xor Tim)
  model += [Mrs_Smith ^ Tim]

  # Either Tim and John will come or neither will come.
  # (Tim = John)
  model += [Tim == John]

  # If Matt comes /\ then John and his father will also come.
  # (Matt -> (John /\ Mr_Smith))
  model += [Matt.implies(John & Mr_Smith)]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print('num_solutions:', num_solutions)


mr_smith_v1()
print()
mr_smith_v2()
