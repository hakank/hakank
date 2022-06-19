"""
Added corner puzzle in cpmpy.

Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
'''
This puzzle requires that you enter the digits 1 through 8 in the circles and
squares (one digit in each figure) so that the number in each square is equal
to the sum on the numbers in the circles which adjoin it.  
...

  C F C
  F   F
  C F C
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def added_corner():

  n = 8
  
  x = intvar(1,n,shape=n,name="x")
  a,b,c,d,e,f,g,h = x
  
  # constraints
  model = Model([AllDifferent(x),
                 b == a + c,
                 d == a + f,
                 e == c + h,
                 g == f + h,
                 ])

  def print_sol():
      print(a.value(),b.value(),c.value())
      print(d.value()," ", e.value())
      print(f.value(),g.value(),h.value())
      print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  


added_corner()
