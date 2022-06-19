"""
Circling the Squares puzzle in cpmpy.

From the Oz examples
http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/circlingsquares.html
'''
from 'Amusements in Mathematics, Dudeney', number 43.

The puzzle is to place a different number in each of the ten squares
so that the sum of the squares of any two adjacent numbers shall be
equal to the sum of the squares of the two numbers diametrically
opposite to them. The four numbers placed, as examples, must stand as
they are. Fractions are not allowed, and no number need contain more
than two figures.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def s(x1,x2,y1,y2):
  return (x1*x1 + x2*x2 == y1*y1 + y2*y2)


def circling_squares():

  n = 10

  # variables
  x = intvar(1,99,shape=n,name="x")
  A,B,C,D,E,F,G,H,I,K = x

  # constraints
  model = Model([AllDifferent(x),
                 A == 16,
                 B == 2,
                 F == 8,
                 G == 14,

                 s(A, B, F, G),
                 s(B, C, G, H),
                 s(C, D, H, I),
                 s(D, E, I, K),
                 s(E, F, K, A),
                 ])
  
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:", num_solutions)

circling_squares()
