"""
Heterosquare problem in cpmpy.

From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
'''
A heterosquare of order n is a n*n square whose elements are distinct integers from 
1 to n^2 such that the sums of the rows, columns and diagonals are all different. 
Here is an example of heterosquare of order 3 

           19

1  2  3    6
8  9  4    21
7  6  5    18

16 17 12   15  (Sums)
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def heterosquare(n=3):
  model = Model()

  # variables
  x = intvar(1,n*n,shape=(n,n),name="x")

  row_sums = intvar(1,n**3,shape=n, name="row_sums")
  col_sums = intvar(1,n**3,shape=n,name="col_sums")

  diag1 = intvar(1,n**3,name="diag1")
  diag2 = intvar(1,n**3,name="diag2")

  # constraints

  # all the entries in the matrix should be different
  model += (AllDifferent(x))

  # and all sums should be different
  model += (AllDifferent(row_sums + col_sums + [diag1, diag2]))

  # calculate rows sums
  for i in range(n):
    model += (row_sums[i] == sum(x[i,:]))

  # calculate column sums
  for j in range(n):
    model += (col_sums[j] == sum(x[:,j]))

  # diag1 sums
  model += (sum([x[i,i] for i in range(n)]) == diag1)

  # diag2 sums
  model += (sum([x[i,n-i-1] for i in range(n)]) == diag2)

  # symmetry breaking
  model += [frenicle(x,n)]

  # print(model)
  def print_sol():
    print("diag1:",diag1.value(),"diag2:",diag2.value())
    print("row_sums:",row_sums.value())
    print("col_sums:",col_sums.value())
    for i in range(n):
      for j in range(n):
        print(x[(i,j)].value(),end=" ")
      print()
    print()

  ss = CPM_ortools(model)
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0  
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

n = 3
heterosquare(n)
