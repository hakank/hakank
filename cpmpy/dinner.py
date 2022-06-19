"""
Dinner problem in cpmpy.

From http://www.sellsbrothers.com/spout/#The_Logic_of_Logic
'''
My son came to me the other day and said, 'Dad, I need help with a
math problem.' The problem went like this:

* We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children
* Grandparents cost $3 for dinner, parents $2 and children $0.50
* There must be 20 total people at dinner and it must cost $20
* How many grandparents, parents and children are going to dinner?
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def dinner():
   
  n = 3

  # variables
  # "We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children"
  grandparents = intvar(1,6,name="grandparents")
  parents = intvar(1,10,name="parents")
  children = intvar(1,40,name="children")    

  model = Model([
    # "Grandparents cost $3 for dinner, parents $2 and children $0.50"
    # "There must be 20 total people at dinner ...
    grandparents * 3 + parents * 2 + children * 1 / 2  == 20,
    # ... and it must cost $20"
    grandparents + parents + children == 20
    ])

  def print_sol():
    print("grandparents:",grandparents.value(), "parents:",parents.value(), "children:",children.value())    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  
  print()

dinner()
