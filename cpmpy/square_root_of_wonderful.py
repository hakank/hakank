"""
Square root of WONDERFUL in cpmpy.
 
Martin Gardner (June 1961)
'''
'The Square Root of Wonderful' was the name of a play on Broadway. If
each letter in WONDERFUL stands for a different digit (zero excluded)
and if OODDF, using the same code, represent the square root, the what
_is_ the square root of wonderful?
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def square_root_of_wonderful():

  # data
  n = 9

  # decision variabels
  x = intvar(1,n,shape=n,name="x")
  W,O,N,D,E,R,F,U,L = x
  
  wonderful = intvar(10**8,10**9,name="wonderful")
  ooddf = intvar(10**4,10**6-1,name="ooddf")

  # constraints
  model = Model([AllDifferent(x),
                 wonderful == 100000000*W + 10000000*O + 1000000*N + 100000*D +
                              10000*E + 1000*R +  100*F + 10*U + L,
                 ooddf == 10000*O + 1000*O + 100*D + 10*D + F,
                 ooddf*ooddf == wonderful,
    ])

  print_model_and_variables(model)

  def print_sol():
    print("wonderful:",wonderful.value())
    print("ooddf:",ooddf.value())      
    print()

  ss = CPM_ortools(model)
  num_solutions = model.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  


square_root_of_wonderful()
