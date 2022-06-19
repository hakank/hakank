"""
Contracting costs in cpmpy.

From 
http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
'''
Contracting Costs from 'Mathematical Puzzles of Sam Loyd, Volume 2',
number 20.

A contractor planning the construction of a house found that he would
have to pay:

  * $ 1,100 to the paper hanger and the painter,
  * $ 1,700 to the painter and plumber,
  * $ 1,100 to the plumber and electrician,
  * $ 3,300 to the electrician and carpenter,
  * $ 5,300 to the carpenter and mason,
  * $ 3,200 to the mason and painter. 

What does each man charge for his services?
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def contracting_costs():

  n = 6
  x = intvar(1,5300,shape=n,name="x")
  paper_hanger, painter, plumber, electrician, carpenter, mason = x

  costs = [[paper_hanger,painter,    1100],
           [painter,     plumber,    1700],
           [plumber,     electrician,1100],
           [electrician, carpenter,  3300],
           [carpenter,   mason,      5300],
           [mason,       painter,    3200],
           ]

  people = ["paper_hanger", "painter", "plumber", "electrician", "carpenter", "mason"]

  # model = Model([1100 == paper_hanger + painter,
  #                1700 == painter + plumber,
  #                1100 == plumber + electrician,
  #                3300 == electrician + carpenter,
  #                5300 == carpenter + mason,
  #                3200 == mason + painter,
  #                ])

  # Alternative encoding:
  model = Model([sum(costs[i][:2]) == costs[i][2] for i in range(n)])

  def print_sol():
    [print(f"{people[i]:15s}: ${x[i].value():5}") for i in range(n)]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

contracting_costs()
