"""
Clock Triplet Problem in MiniZinc.
 
Problem formulation
http://www.f1compiler.com/samples/Dean%20Clark%27s%20Problem.f1.html
'''
Dean Clark's Problem (Clock Triplets Problem)

The problem was originally posed by Dean Clark and then presented
to a larger audience by Martin Gardner. 

The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
by Timothy Rolfe. According to the article, in his August 1986 column for 
Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
 
   Now for a curious little combinatorial puzzle involving the twelve
   numbers on the face of a clock. Can you rearrange the numbers (keeping
   them in a circle) so no triplet of adjacent numbers has a sum higher 
   than 21? This is the smallest value that the highest sum of a triplet
   can have.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def show(x):
  return x.value()

def clock_triplets():

  n = 12

  # variables
  x = intvar(1,n, shape=n,name="x")
  triplet_sum = intvar(0,21,name="triplet_sum")

  # constraints
  model = Model([AllDifferent(x),
                 x[0] == 12, 
                 x[1] > x[11],
                 [(x[i%12] + x[(i%12)-1] + x[(i%12)-2]) <= triplet_sum for i in range(n)],
                 ])

  def print_sol():
    print("triplet_sum:",triplet_sum.value())
    print("       ", show(x[0]), "\n")
    print("     ", show(x[11]), "    ", show(x[1]), "\n")
    print("   ", show(x[10]), "       ", show(x[2]), "\n")
    print("  ", show(x[9]), "         ", show(x[3]), "\n")
    print("   ", show(x[8]), "        ",show(x[4]), "\n")
    print("     ",  show(x[7]), "    ", show(x[5]), "\n")
    print("       ", show(x[6]), "\n")

  
  num_solutions = model.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)


clock_triplets()


