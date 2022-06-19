"""
Ages of the sons puzzle in cpmpy.

From 'Drools Puzzle Round 1: Ages of the Sons'
http://blog.athico.com/2007/08/drools-puzzle-round-1-ages-of-sons.html
'''
An old man asked a mathematician to guess the ages of his three sons.

Old man said: “The product of their ages is 36.”
Mathematician said: “I need more information.”

Old man said:”Over there you can see a building. The sum of their ages
equals the number of the windows in that building.”
After a short while the mathematician said: “I need more information.”

Old man said: “The oldest son has blue eyes.”
Mathematician said: “I got it.”

What are the ages of the three sons of the old man?
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort


def ages_of_the_sons():

  A1 = intvar(0,36,name="A1") # oldest son
  A2 = intvar(0,36,name="A2")
  A3 = intvar(0,36,name="A3")

  B1 = intvar(0,36,name="B1")
  B2 = intvar(0,36,name="B2")
  B3 = intvar(0,36,name="B3")
  AS = intvar(0,1000,name="AS")
  BS = intvar(0,1000,name="BS")  

  model = Model([A1 > A2,
                 A2 >= A3,
                 36 == A1 * A2 * A3,
                 
                 B1 >= B2,
                 B2 >= B3,
                 A1 != B1,
                 
                 36 == B1 * B2 * B3,
                 AS == A1 + A2 + A3,
                 BS == B1 + B2 + B3,
                 AS == BS
                ])

  def print_sol():
    print([A1.value(),A2.value(),A3.value()])
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)
  
ages_of_the_sons()
