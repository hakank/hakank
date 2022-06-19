"""
Book buy puzzle in cpmpy.

From Martin Chlond Integer Programming Puzzles:
http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 9.
Source:  M Kraitchik, 'Mathematical Recreations', p37, Dover

This model was inspired by the AMPL model created by Martin Chlond.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def book_buy():
  
  # fathers: 1 = Peter, 2 = Paul, 
  # sons: 1 = Tom, 2 = Dick
  m = 2

  # w = 1 if Peter is Tom's father, 0 otherwise 
  w = boolvar(name="w")

  # number of books (and price) bought by father i
  x = intvar(1,8,shape=m,name="x")  
  # number of books (and price) bought by son j
  y = intvar(1,8,shape=m,name="y")  

  model = Model([y[1] == 1,      # Dick buys one book
                 x[0] == y[0]+1, # Peter buys one more book than Tom
                 
                 # each family spends $65
                 x[0]*x[0] + w*y[0]*y[0] + (1-w)*y[1]*y[1] == 65,
                 x[1]*x[1] + (1-w)*y[0]*y[0] + w*y[1]*y[1] == 65
                 ])

  def print_sol():
    ww = w.value()
    print("w:", ww)
    print(["Peter","Paul"][ww], "is Dick's father")
    print("x:", x.value())
    print("y:", y.value())


  ss = CPM_ortools(model)
  ss.solveAll(display=print_sol)


book_buy()

