"""
Pigeon hole problem in cpmpy.

ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
'''
pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes (at most 1
pigeon per hole). The boolean formulation uses n × m variables to indicate, for each pigeon,
its hole number. Obviously, there is a solution iff n <= m.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

# n: num pigeons
# m: n pigeon holes
def pigeon_hole(n=3,m=10):
  model = Model()

  # variables
  p = boolvar(shape=(n,m),name="p")
  print("p:",p)

  # max 1 pigeon per pigeon hole
  for j in range(m):
    model += (sum([p[(i,j)] for i in range(n)]) <= 1)

  # all pigeon must be placed and only at one hole
  for i in range(n):
    model += (sum([p[(i,j)] for j in range(m)]) == 1)

  def print_sol():
    print(p.value())
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

n = 3
m = 10
pigeon_hole(n,m)
