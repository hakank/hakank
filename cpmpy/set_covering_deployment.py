"""
Set covering deployment in cpmpy.

From http://mathworld.wolfram.com/SetCoveringDeployment.html
'''
Set covering deployment (sometimes written 'set-covering deployment'
and abbreviated SCDP for 'set covering deployment problem') seeks
an optimal stationing of troops in a set of regions so that a
relatively small number of troop units can control a large
geographic region. ReVelle and Rosing (2000) first described
this in a study of Emperor Constantine the Great's mobile field
army placements to secure the Roman Empire.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def set_covering_deployment():
 
  # data
  countries = [
      "Alexandria", "Asia Minor", "Britain", "Byzantium", "Gaul", "Iberia",
      "Rome", "Tunis"
  ]
  n = len(countries)

  # the incidence matrix (neighbours)
  mat = [[0, 1, 0, 1, 0, 0, 1, 1], [1, 0, 0, 1, 0, 0, 0, 0],
         [0, 0, 0, 0, 1, 1, 0, 0], [1, 1, 0, 0, 0, 0, 1, 0],
         [0, 0, 1, 0, 0, 1, 1, 0], [0, 0, 1, 0, 1, 0, 1, 1],
         [1, 0, 0, 1, 1, 1, 0, 1], [1, 0, 0, 0, 0, 1, 1, 0]]

  # declare variables

  # First army
  X = boolvar(shape=n,name="X") 

  # Second (reserv) army
  Y = boolvar(shape=n,name="Y")

  num_armies = intvar(0,n,name="num_armies")

  model = Model(minimize=num_armies)

  # constraints

  # total number of armies
  # model += [num_armies == sum([X[i] + Y[i] for i in range(n)])]
  model += [num_armies == np.sum(X + Y)] 

  #
  #  Constraint 1: There is always an army in a city
  #                (+ maybe a backup)
  #                Or rather: Is there a backup, there
  #                must be an an army
  #
  # model += [X[i] >= Y[i] for i in range(n)]
  model += [X >= Y]  

  #
  # Constraint 2: There should always be an backup army near every city
  #
  for i in range(n):
    model += [X[i] + sum([Y[j] for j in range(n) if mat[i][j] == 1]) >= 1]


  ss = CPM_ortools(model)
  if ss.solve():
    print("num_armies:", num_armies.value())
    print("X:", X.value())
    print("Y:", Y.value())

    for i in range(n):
      if X[i].value() == 1:
        print("army:", countries[i], end=" ")
      if Y[i].value() == 1:
        print("reserv army:", countries[i], " ")
    print()

  print()


set_covering_deployment()
