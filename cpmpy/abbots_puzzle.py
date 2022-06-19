"""
Abbot's puzzle in cpmpy.

http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
'''
The Abbot's Puzzle from 'Amusements in Mathematics, Dudeney', number 110.

If 100 bushels of corn were distributed among 100 people in such a
manner that each man received three bushels, each woman two, and each
child half a bushel, how many men, women, and children were there?

Dudeney added the condition that there are five times as many women as
men. That way, the solution becomes unique (otherwise, there are seven
solutions).
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def abbots_puzzle():

  model = Model()

  x = intvar(0,100,shape=3,name="x")
  M, W, C = x

  model = Model([100 == M + W + C,
                 M * 6 + W * 4 + C == 200,
                 M * 5 == W
                 ])

  def print_sol():
    print("M:",M.value(),"W:",W.value(),"C:",C.value())    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  

abbots_puzzle()
