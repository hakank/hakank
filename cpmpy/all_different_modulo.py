"""
All different modulo in cpmpy.

From Global Constraint Catalogue
http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_modulo.html
'''
Enforce all variables of the collection VARIABLES to have a distinct 
rest when divided by M.

Example
(<25, 1,14, 3>, 5)

The equivalence classes associated with values 25, 1, 14 and 3 are 
respectively equal to 
   25 mod 5 = 0, 1 mod 5 = 1, 14 mod 5 = 4 and 3 mod 5 = 3. 
Since they are distinct the alldifferent_modulo constraint holds.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def all_different_modulo_test(n=4,m=5):
 
  # variables
  x = intvar(1,25,shape=n,name="x")
  print("x1:",x)

  # constraints
  model = Model([all_different_modulo(x, m)])

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print()
  print("num_solutions:", num_solutions)


n=4
m=5
all_different_modulo_test(n,m)
