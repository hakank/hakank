"""
Test of argmax* and argmin* in cpmpy

- argmin
- argmax
- argmin_except_0 (which use argmin_except_c)
- argmax_except_c


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def argmax_test():

  n = 5

  # variables
  x = intvar(0,n,shape=n,name="x")
  z_max = intvar(0,n-1,name="z_max")
  z_min = intvar(0,n-1,name="z_min")
  z_min_except_0 = intvar(0,n-1,name="z_min_except_0")
  z_max_except_n = intvar(0,n-1,name="z_max_except_n")      

  # constraints
  model = Model([# AllDifferent(x),
                 ## z == np.argmax(x) # I had hope that this would work, but it doesn't
                 argmax(x,z_max),
                 argmin(x,z_min),
                 argmin_except_0(x,z_min_except_0),
                 argmax_except_c(x,z_max_except_n,n),                 
                 atleast(x,0,1), # at least some 0
                 atleast(x,n,1), # at least some n
                 ])

  def print_sol():
    print("x:",x.value())
    print("z_max:",z_max.value())
    print("z_min:",z_min.value())
    print("z_min_except_0:",z_min_except_0.value())
    print("z_max_except_n:",z_max_except_n.value())            
    print("x[z_max]:",x[z_max.value()].value())
    print("x[z_min]:",x[z_min.value()].value())
    print("x[z_min_except_0]:",x[z_min_except_0.value()].value())
    print("x[z_max_except_n]:",x[z_max_except_n.value()].value())            
    print(flush=True)


  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


argmax_test()


