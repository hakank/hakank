"""
Test of constraints value_precede and value_precede_chain in cpmpy

The global constraint 
   value_precede(s,t, x) 
ensures that the value s precedesl the value t in array x
if both s and t are in x.

The global constraint
      value_precede_chain(c, x)
ensures that the value c[i-1] precedes the value c[i] is the array x
if both c[i-1] and c[i] are in x.

These constraints are often used for symmetry breaking.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
import operator


#
# Testing value_precede
#
def value_precede_test(n):
    

  # variables
  x = intvar(1,n,shape=n,name="x")
  
  # constraints
  model = Model([value_precede(4,3,x)])
  
  num_solutions = model.solveAll(display=x)
  print("num_solutions:",num_solutions)


#
# Testing value_precede_chain
#
def value_precede_chain_test(n):

  # variables
  x = intvar(1,n,shape=n,name="x")

  cs = list(range(1,n+1))
  print("cs:",cs),
  
  # constraints
  model = Model([value_precede_chain(cs,x)])
  num_solutions = model.solveAll(display=x)
  print("num_solutions:",num_solutions)



n = 4
print("value_precede_test")
value_precede_test(n)

print("\nvalue_precede_chain_test")
value_precede_chain_test(n)


