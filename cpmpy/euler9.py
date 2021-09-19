"""
Project Euler problem 9 in cpmpy.
'''
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def euler9():

   x = intvar(1,500,shape=3,name="x")

   model = Model([sum(x) == 1000,
                x[0]*x[0] + x[1]*x[1] == x[2]*x[2],
                # symmetry breaking
                x[0] <= 1000//3,
                increasing(x)
                ])

   ss = CPM_ortools(model)
   num_solutions = 0
   sol = []
   if ss.solve():
      num_solutions += 1
      print(prod1(x.value()))


euler9()
