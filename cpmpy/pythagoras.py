"""
Pythagoras in cpmpy.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,re
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def pythagoras():

  model = Model()

  x = intvar(1,1000,shape=3,name="x")

  model = Model([
      # This don't work:
      # Not a know supported ORTools left-hand-side 'pow' ((x[0]) pow 2) == (IV1)
      # x[0]**2 + x[1]**2 == x[2]**2,
      
      x[0]*x[0] + x[1]*x[1] == x[2]*x[2], # This works
      increasing(x)
      ])

  model.solveAll(display=x)


pythagoras()
