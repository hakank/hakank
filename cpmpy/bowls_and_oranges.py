"""
Bowls and Oranges problem in cpmpy.

From BitTorrent Developer Challenge
http://www.bittorrent.com/company/about/developer_challenge
'''
You have 40 bowls, all placed in a line at exact intervals of 
1 meter. You also have 9 oranges. You wish to place all the oranges 
in the bowls, no more than one orange in each bowl, so that there are 
no three oranges A, B, and C such that the distance between A and B is 
equal to the distance between B and C. How many ways can you arrange 
the oranges in the bowls?.
'''

Via http://surana.wordpress.com/2011/06/01/constraint-programming-example/

There are 7555794 ways.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def bowls_and_oranges():

  # 7555794 using increasing as symmetry breaking
  n = 40 # number of bowls
  m = 9  # number of oranges

  # There are 3452 different solutions to this configuration
  # n = 20 # number of bowls
  # m = 4  # number of oranges

  x = intvar(1,n,shape=m,name="x")

  model = Model([AllDifferent(x),
                increasing(x)
                 ])

  for i in range(m):
    for j in range(m):
        for k in range(m):
            if i < j and j < k:
                model += ((x[j]-x[i]) != (x[k]-x[j]))

  # num_solutions = model.solveAll(display=x)
  num_solutions = model.solveAll()  
  print("num_solutions:", num_solutions)

bowls_and_oranges()

