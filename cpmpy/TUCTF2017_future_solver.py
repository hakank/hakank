"""
TUCTF 2017 Future solver in cpmpy

This is a port of the Z3 model in:
https://gist.github.com/MariaRigaki/5b4361b056380bae6d78c91bc4552d57
'''
A solution to the CUCTF 2017 Future task using Z3Py
inspired by Hackeriet's solution in https://blog.hackeriet.no/solution-to-tuctf-2017-future/
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from collections import defaultdict



def TUCTF2017_future_solver():

  model = Model()


  # This is the flag format
  # flag = "TUCTF{..................}"
  
  auth = [0x8b, 0xce, 0xb0, 0x89, 0x7b, 0xb0, 0xb0, 0xee, 0xbf, 0x92, 0x65, 0x9d, 0x9a, 0x99, 0x99, 0x94, 0xad, 0xe4, 0x00]

  # Declare the variables
  a, b, c, d, e, f, g, h, i, j, k, l, m = intvar(0,126,shape=13)
  n, o, p, q, r, s, t, u, v, w, x, y, z = intvar(0,126,shape=13)

  mat = [[a, b, c, d, e], [f, g, h, i, j], [k, l, m, n, o], [p, q, r, s, t], [u, v, w, x, y]]

  # Create the equations based on genAuthString()
  equations = [mat[0][0] + mat[4][4] == int(auth[0]),
               mat[2][1] + mat[0][2] == int(auth[1]),
               mat[4][2] + mat[4][1] == int(auth[2]),
               mat[1][3] + mat[3][1] == int(auth[3]),
               mat[3][4] + mat[1][2] == int(auth[4]),
               mat[1][0] + mat[2][3] == int(auth[5]),
               mat[2][4] + mat[2][0] == int(auth[6]),
               mat[3][3] + mat[3][2] + mat[0][3] == int(auth[7]),
               mat[0][4] + mat[4][0] + mat[0][1] == int(auth[8]),
               mat[3][3] + mat[2][0] == int(auth[9]),
               mat[4][0] + mat[1][2] == int(auth[10]),
               mat[0][4] + mat[4][1] == int(auth[11]),
               mat[0][3] + mat[0][2] == int(auth[12]),
               mat[3][0] + mat[2][0] == int(auth[13]),
               mat[1][4] + mat[1][2] == int(auth[14]),
               mat[4][3] + mat[2][3] == int(auth[15]),
               mat[2][2] + mat[0][2] == int(auth[16]),
               mat[1][1] + mat[4][1] == int(auth[17]),
               mat[0][0] == ord('T'),
               mat[2][1] == ord('U'),
               mat[4][2] == ord('C'),
               mat[1][3] == ord('T'),
               mat[3][4] == ord('F'),
               mat[1][0] == ord('{'),
               mat[2][4] == ord('}')]

  # Create a solver and solve
  model = Model(equations)

  def print_sol():
    # Map the solved values to the flag
    # Create a mapping dictionary based on genMatrix()
    mapper = {}
    for i in range(25):
      m = (i*2)%25
      f = (i*7)%25
      mapper[f] = mat[int(m/5)][m%5].value()
    print("mapper:",mapper)
    
    solution = [mapper[ix] for ix in range(25)]
    print("solution:", ''.join(chr(int(i)) for i in solution))
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

TUCTF2017_future_solver()
