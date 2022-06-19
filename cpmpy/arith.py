"""
Global constraint arith in cpmpy.

From Global Constraint Catalogue
http://www.emn.fr/x-info/sdemasse/gccat/Carith.html
'''
Enforce for all variables var of the VARIABLES collection to have 
   var RELOP VALUE.

Example
(<4, 5, 7, 4, 5>, <, 9)

The arith constraint holds since all values of the collection 
<4, 5, 7, 4, 5> are strictly less than 9.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort


def arith_test():

  relops = ["<","<=","=",">=",">","!="]

  n = 5
  x = intvar(0,4,shape=n,name="x")
  y = intvar(0,9,name="y")
  relop = intvar(0,len(relops)-1,name="relop")


  # constraints
  model = Model([y <= 3,
                 arith(x, relop, y)
                 ])

  def print_sol():
    relops = ["<","<=","=",">=",">","!="]
    print(x.value(), relops[y.value()], y.value())

  num_solutions = model.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


arith_test()
