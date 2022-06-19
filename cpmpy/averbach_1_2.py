"""
Circular table problem in cpmpy.

From Averbach & Chein 'Problem Solving Through Recreational Mathematics', 
page 2, problem 1.2

'''
Ms X, Ms Y, and Ms Z - and American woman, and Englishwoman, and a Frenchwoman, but not
neccessarily in that order, were seated around a circular table, playing a game of Hearts.
Each passed three cards to the person on her right.
Ms Y passed three hearts to the American, 
Ms X passed the queen of spades and two diamonds to the person who passed her cards
to the Frenchwoman

Who was the American? The Englishwoman? The Frenchwoman?
'''
This model gives the following solution
table: [1, 2, 3]
[American, English, French]: [1, 3, 2]

        1                      American
                     
    3      2               English   French

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort


def averbach_1_2():
  # a is right to b
  def right_to(a, b):
    # return ((a == b+1) | (a == b-2) )
    return (a == (b+1) % 3)

  # a is left to b
  def left_to(a, b):
    return [right_to(b,a)]


  n = 3
  # variables
  xtable = intvar(0,2,shape=n,name="xtable")
  x,y,z = xtable
  
  women = intvar(0,2,shape=n,name="women")
  american,english,french = women
  women_s = ["american","english","french"]

  model = Model([AllDifferent(xtable),
                 AllDifferent([american,english,french]),
                 right_to(y,american),
                 left_to(x,french),

                 # symmetry breaking
                 x == 0
                 ])

  def print_sol():
    print("women :", women.value())
    print("xtable:", xtable.value())
    print("seating:", [women_s[j] for i in range(n) for j in range(n) if women[j].value() == i ])    
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)




averbach_1_2()
