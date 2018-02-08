#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Photo problem in Z3
#
# Problem statement from Mozart/Oz tutorial:
# http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
# '''
# Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one
# row for taking a photo. Some of them have preferences next to whom
# they want to stand:
#
#    1. Betty wants to stand next to Gary and Mary.
#    2. Chris wants to stand next to Betty and Gary.
#    3. Fred wants to stand next to Mary and Donald.
#    4. Paul wants to stand next to Fred and Donald.

# Obviously, it is impossible to satisfy all preferences. Can you find
# an alignment that maximizes the number of satisfied preferences?
# '''
#
# Oz solution:
#   6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
# [5, 6, 1, 3, 7, 4, 2]

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main(show_all_max=0):

  sol = Solver()

  #
  # data
  #
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
      [0, 0, 1, 1, 0, 0, 0]  # Paul   6
  ]

  print("""Preferences:
     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.
    """)

  #
  # declare variables
  #
  positions = [makeIntVar(sol, "positions[%i]" % i, 0, n - 1) for i in range(n)]

  # successful preferences
  z = makeIntVar(sol, "z", 0, n * n)

  #
  # constraints
  #
  sol.add(Distinct(positions))

  # calculate all the successful preferences
  sol.add(z == Sum([If(Abs(positions[i] - positions[j])== 1,1,0)
       for i in range(n) for j in range(n) if preferences[i][j] == 1]))

  #
  # Symmetry breaking (from the Oz page):
  #   Fred is somewhere left of Betty
  sol.add(positions[3] < positions[0])

  # objective
  # objective = sol.Maximize(z, 1)
  if show_all_max != 0:
    print("Showing all maximum solutions (z == 6).\n")
    sol.add(z == 6)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("z:", mod.eval(z))
    p = [mod.eval(positions[i]).as_long() for i in range(n)]

    print(" ".join([persons[j]
                    for i in range(n) for j in range(n) if p[j] == i]))
    print("Successful preferences:")
    for i in range(n):
      for j in range(n):
        if preferences[i][j] == 1 and Abs(p[i] - p[j]) == 1:
          print("\t", persons[i], persons[j])
    print()
    getGreaterSolution(sol,mod,z)

  print()
  print("num_solutions:", num_solutions)


show_all_max = 0  # show all maximal solutions
if __name__ == "__main__":
  if len(sys.argv) > 1:
    show_all_max = 1
  main(show_all_max)
