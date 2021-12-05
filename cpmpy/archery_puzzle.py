"""
Archery puzzle in cpmpy.

http://www.eonhq.com/m/puzzles/images/archery-puzzle.jpg
Archery puzzle by Sam Loyd:
'''
How close can the young archer come to scoring a total of
100 - using as many arrows as she please.
[The targets are: 16, 17, 23, 24, 39, 40]
'''
Via: The Aperiodical: 'Manchester MathsJam June 2012 Recap'
http://aperiodical.com/2012/06/manchester-mathsjam-june-2012-recap/


The solution:

  z: 100
  d: 0
  2 hits on score 16
  4 hits on score 17

x should be: [2,4,0,0,0,0]

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from cpmpy.solvers import *
# from ortools.sat.python import cp_model as ort


def archery_puzzle():
  
  targets = [16, 17, 23, 24, 39, 40]
  n = len(targets)

  target = 100

  # variables
  x = intvar(0,100,shape=n,name="x")
  z = intvar(0,100,name="z")
  d = intvar(0,100,name="d")

  # constraints
  model = Model([z == sum(x*targets),
                 d == abs(target-z)
                 ],
                minimize=d
                )

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("z:", z.value())
    print("d:", d.value())
    xx = x.value()
    print("x:",xx)
    print("\n".join(["%i hits on score %i" % (xx[i], targets[i]) for i in range(n) if xx[i] > 0]))
    print()


  print("num_solutions:", num_solutions)


archery_puzzle()
