"""
Olympic puzzle in cpmpy.

Benchmark for Prolog (BProlog)
'''
File   : olympic.pl
Author : Neng-Fa ZHOU
Date   : 1993

Purpose: solve a puzzle taken from Olympic Arithmetic Contest

Given ten variables with the following configuration:

  X7   X8   X9   X10

    X4   X5   X6

      X2   X3

        X1

We already know that X1 is equal to 3 and want to assign each variable
with a different integer from {1,2,...,10} such that for any three
variables
  Xi   Xj

    Xk
the following constraint is satisfied:

  |Xi-Xj| = Xk
'''

 
This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def minus(x, y, z):
  return [z == abs(x - y)]


def olympic():

  model = Model()

  #
  # data
  #
  n = 10

  #
  # declare variables
  #
  Vars = intvar(1,n,shape=n,name="Vars")
  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10 = Vars

  #
  # constraints
  #
  model += [AllDifferent(Vars)]

  model += [X1 == 3]
  model += [minus(X2, X3, X1)]
  model += [minus(X4, X5, X2)]
  model += [minus(X5, X6, X3)]
  model += [minus(X7, X8, X4)]
  model += [minus(X8, X9, X5)]
  model += [minus(X9, X10, X6)]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=Vars)
  print('num_solutions:', num_solutions)

olympic()
