"""
Balancing on a seesaw in cpmpy.

From  Marriott & Stuckey 'Programming with Constraints', page 257.
'''
Suppose that Liz, Fi, and Sarah are playing on a 10 foot long seesaw
which has seats placed uniformly one foot apart along the bar. ...
They wish to position themselves on the seesaw to that it balances.
The also wish to be able to swing their arms freely requiring that
they are at least three feet apart. The weights of Liz, Fi, and Sarah
are respectively 9, 8, and 4 stone.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


# ensure that x and y are n feet apart
def apart(x, y, n):
  return ((x >= y + n) | (y >= x + n))

def stuckey_seesaw():
 
  n = 3

  # variables
  people = intvar(-5,5,shape=n,name="people")
  Liz, Fi, Sara = people
  Liz.name="Liz"
  Fi.name="Fi"
  Sara.name="Sara"

  model = Model([
    # constraints
    9*Liz + 8* Fi + 4*Sara == 0,
    apart(Liz, Fi, 3),
    apart(Liz, Sara, 3),
    apart(Sara, Fi, 3),
    # Sara is at the far right
    Sara == max(people) 
    # symmetry breaking
    # Sara >= 0

    ])

  def print_sol():
    print([(p,p.value()) for p in people])    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

stuckey_seesaw()


