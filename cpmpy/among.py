"""
Global constraint among in cpmpy.
'''
Requires exactly m variables in x to take one of the values in v.
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def among_test():

  n = 5 # length of x
  m = 3 # number of values
  v = [1,5,8]

  # variables
  x = intvar(1,8,shape=n,name="x")

  # constraints  
  model = Model(among(m, x,v))

  ortools_wrapper2(model,[x])


among_test()
