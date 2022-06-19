"""
A puzzle in OR-tools cpmpy.

From the 'God plays dice' blog post 'A puzzle':
http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
And the sequel 'Answer to a puzzle'
http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/

This problem instance was taken from the latter blog post.
'''
8809 = 6
7111 = 0
2172 = 0
6666 = 4
1111 = 0
3213 = 0
7662 = 2
9312 = 1
0000 = 4
2222 = 0
3333 = 0
5555 = 0
8193 = 3
8096 = 5
7777 = 0
9999 = 4
7756 = 1
6855 = 3
9881 = 5
5531 = 0

2581 = ?
'''

Note: 
This instance yields 10 solutions, since x4 is not 
restricted in the constraints. 
All solutions has x assigned to the correct result. 


The problem stated in "A puzzle" (the first blog post)
http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
is
'''
8809 = 6
7662 = 2
9312 = 1
8193 = 3
8096 = 5
7756 = 1
6855 = 3
9881 = 5

2581 = ?
'''
This problem instance - using the same principle - yields 
two different solutions of x, one is the same (correct) as 
for the above problem instance, and one is not.
It's because here both x4 and x1 are underdefined.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def puzzle(problem=1):
  
  print("Problem", problem)
    
  model = Model()

  n = 10

  # variables
  all = intvar(0,9,shape=n,name="all")
  x0,x1,x2,x3,x4,x5,x6,x7,x8,x9 = all

  x = intvar(0,9,name="x") # The unknown

  # Force all variables (especially x4) to have
  # some value.
  for a in all:
    model += (a >= 0)

  if problem==1:
    model += (x8+x8+x0+x9 == 6)
    model += (x7+x1+x1+x1 == 0)
    model += (x2+x1+x7+x2 == 0)
    model += (x6+x6+x6+x6 == 4)
    model += (x1+x1+x1+x1 == 0)
    model += (x3+x2+x1+x3 == 0)
    model += (x7+x6+x6+x2 == 2)
    model += (x9+x3+x1+x2 == 1)
    model += (x0+x0+x0+x0 == 4)
    model += (x2+x2+x2+x2 == 0)
    model += (x3+x3+x3+x3 == 0)
    model += (x5+x5+x5+x5 == 0)
    model += (x8+x1+x9+x3 == 3)
    model += (x8+x0+x9+x6 == 5)
    model += (x7+x7+x7+x7 == 0)
    model += (x9+x9+x9+x9 == 4)
    model += (x7+x7+x5+x6 == 1)
    model += (x6+x8+x5+x5 == 3)
    model += (x9+x8+x8+x1 == 5)
    model += (x5+x5+x3+x1 == 0)
    
    model += (x2+x5+x8+x1 == x)

  else:
    # This yields two different values for x
    model += (x8+x8+x0+x9 == 6)
    model += (x7+x6+x6+x2 == 2)
    model += (x9+x3+x1+x2 == 1)
    model += (x8+x1+x9+x3 == 3)
    model += (x8+x0+x9+x6 == 5)
    model += (x7+x7+x5+x6 == 1)
    model += (x6+x8+x5+x5 == 3)
    model += (x9+x8+x8+x1 == 5)
    
    model += (x2+x5+x8+x1 == x)

  xs = []
  def print_sol():
    print("all:",all.value(), "x:",x.value())
    xs.append(x.value())

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  
  print("xs:",xs)
  print()

puzzle(1)
print()
puzzle(2)
