"""
Rabbit problem in cpmpy.

From http://www.cs.kuleuven.ac.be/~dtai/publications/files/21836.ps.gz
'Constraint logic programming: applications and implementation', page 2 (footnote)
'''
9 animals, rabbits and pheasants are playing on the grass.
We can see 24 legs. How many rabbits and pheasants are there?
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def rabbit():

  rabbits = intvar(0,9,name="rabbit")
  pheasants = intvar(0,9,name="pheasants")

  model = Model([rabbits + pheasants == 9,
                 4*rabbits + 2 * pheasants == 24
                 ])

  def print_sol():
    print("rabbits:", rabbits.value())
    print("pheasants:", pheasants.value())        

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


rabbit()
