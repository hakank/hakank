"""
Fractions problem in cpmpy.
Prolog benchmark problem (BProlog)
'''
Find distinct non-zero digits such that the following equation holds:
       A        D        G
    ------  + ----- + ------  = 1
      B*C      E*F      H*I
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def fractions_problem():

  n = 9
  x = intvar(1,n,shape=9, name="x")
  A,B,C,D,E,F,G,H,I = x

  D1 = intvar(1,n*n,name="D1")
  D2 = intvar(1,n*n,name="D2")
  D3 = intvar(1,n*n,name="D3")

  model = Model([AllDifferent(x),
                 D1 == 10*B+C,
                 D2 == 10*E+F,
                 D3 == 10*H+I,
                 A*D2*D3 + D*D1*D3 + G*D1*D2 == D1*D2*D3,
                 # break the symmetry
                 A*D2 >= D*D1,
                 D*D3 >= G*D2,
                 # redundant constraints
                 3*A >= D1,
                 3*G <= D2,
                 ])

  def print_sol():
    print("%s/(%s%s) + %s(%s%s) + %s(%s%s) = 1" %( A.value(),B.value(), C.value(),D.value(),
                                                   E.value(),F.value(),G.value(),H.value(),
                                                   I.value()))
    print()
   
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

fractions_problem()
