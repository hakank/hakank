"""
Test of decomposition of minimum_except_0 in cpmpy.

 This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
from cpmpy.solvers.utils import get_supported_solvers
import numpy as np
from cpmpy_hakank import *

def minimum_except_0_test(n):

  x = intvar(0,n, shape=n,name="x")
  z = intvar(0,n,name="z")

  model = Model([atleast(x,0,n-1), 
                 # minimum_except_c(x,z,0,True), # [0,0,0,0] is a solution
                 minimum_except_c(x,z,0), # [0,0,0,0] is NOT a solution
                ])
  
  print("model:", model)

  def print_sol():
      print("x:",x.value(),"z:",z.value(),flush=True)
      
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


minimum_except_0_test(4)

