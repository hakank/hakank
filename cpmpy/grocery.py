"""
Grocery problem in cpmpy.

From  Christian Schulte, Gert Smolka, Finite Domain
http://www.mozart-oz.org/documentation/fdt/
Constraint Programming in Oz. A Tutorial. 2001.
'''
A kid goes into a grocery store and buys four items. The cashier
charges $7.11, the kid pays and is about to leave when the cashier
calls the kid back, and says 'Hold on, I multiplied the four items
instead of adding them; I'll try again; Hah, with adding them the
price still comes to $7.11'. What were the prices of the four items?
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def grocery():

  model = Model()

  # data
  n = 4
  c = 711

  # variables
  item = intvar(0,c,shape=n,name="item")

  # constraints
  model += [sum(item) == c]
  model += [prod1(item) == c*100**3]

  # symmetry breaking
  model += [increasing(item)]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=item)
  print("num_solutions:", num_solutions)


grocery()
