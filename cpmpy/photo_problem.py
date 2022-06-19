"""
Photo problem in cpmpy.

Problem statement from Mozart/Oz tutorial:
http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
'''
Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one
row for taking a photo. Some of them have preferences next to whom
they want to stand:

  1. Betty wants to stand next to Gary and Mary.
  2. Chris wants to stand next to Betty and Gary.
  3. Fred wants to stand next to Mary and Donald.
  4. Paul wants to stand next to Fred and Donald.

Obviously, it is impossible to satisfy all preferences. Can you find
an alignment that maximizes the number of satisfied preferences?
'''

Oz solution:
  6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def photo_problem(z_val=0):
  print("z_val:", z_val)

  # data
  persons = ["Betty", "Chris", "Donald", "Fred", "Gary", "Mary", "Paul"]
  n = len(persons)
  preferences = [
      # 0 1 2 3 4 5 6
      # B C D F G M P
      [0, 0, 0, 0, 1, 1, 0],  # Betty  0
      [1, 0, 0, 0, 1, 0, 0],  # Chris  1
      [0, 0, 0, 0, 0, 0, 0],  # Donald 2
      [0, 0, 1, 0, 0, 1, 0],  # Fred   3
      [0, 0, 0, 0, 0, 0, 0],  # Gary   4
      [0, 0, 0, 0, 0, 0, 0],  # Mary   5
      [0, 0, 1, 1, 0, 0, 0]   # Paul   6
  ]

  if z_val == 0:
      print("""Preferences:
         1. Betty wants to stand next to Gary and Mary.
         2. Chris wants to stand next to Betty and Gary.
         3. Fred wants to stand next to Mary and Donald.
         4. Paul wants to stand next to Fred and Donald.
        """)
      
  #
  # declare variables
  #
  positions = intvar(0,n-1,shape=n,name="positions")

  # successful preferences
  z = intvar(0, n * n, name="z")

  if z_val > 0:
      model = Model(z == z_val)
  else:
      model = Model(maximize=z)

  #
  # constraints
  #
  model += [AllDifferent(positions)]
  model += [z == sum([abs(positions[i]-positions[j]) == 1
                             for i in range(n)
                             for j in range(n)
                             if preferences[i][j] == 1])]

  # Symmetry breaking (from the Oz page):
  #   Fred is somewhere left of Betty
  model += [positions[3] < positions[0]]

  def print_sol():
    print("z:", z.value())
    p = [positions[i].value() for i in range(n)]
    print("p:",p)

    print(" ".join(
        [persons[j] for i in range(n) for j in range(n) if p[j] == i]))
    print("Successful preferences:")
    for i in range(n):
      for j in range(n):
        if preferences[i][j] == 1 and abs(p[i] - p[j]) == 1:
          print("\t", persons[i], persons[j])
    print()
    

  num_solutions = 0
  ss = CPM_ortools(model)
  if z_val == 0:
    ss.solve()
    return z.value()
  else:
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)


z = photo_problem(0)
print(f"Max value is {z}. Find all optimal solutions:")
photo_problem(z)

