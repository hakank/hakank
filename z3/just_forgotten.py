#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Just forgotten puzzle (Enigma 1517) in Z3
#
# From http://www.f1compiler.com/samples/Enigma 201517.f1.html
# '''
# Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
#
# Joe was furious when he forgot one of his bank account numbers.
# He remembered that it had all the digits 0 to 9 in some order,
# so he tried the following four sets without success:
#
#     9 4 6 2 1 5 7 8 3 0
#     8 6 0 4 3 9 1 2 5 7
#     1 6 4 0 2 9 7 8 5 3
#     6 8 2 4 3 1 9 0 7 5
#
# When Joe finally remembered his account number, he realised that
# in each set just four of the digits were in their correct position
# and that, if one knew that, it was possible to work out his
# account number. What was it?
# '''
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = SolverFor("QF_FD")

  # data
  rows = 4
  cols = 10
  # The four tries
  a = [[9, 4, 6, 2, 1, 5, 7, 8, 3, 0],
       [8, 6, 0, 4, 3, 9, 1, 2, 5, 7],
       [1, 6, 4, 0, 2, 9, 7, 8, 5, 3],
       [6, 8, 2, 4, 3, 1, 9, 0, 7, 5]]

  # variables
  x = [makeIntVar(sol, "x[%i]" % j, 0, 9) for j in range(cols)]

  #
  # constraints
  #
  sol.add(Distinct(x))

  for r in range(rows):
    sol.add(Sum([If(x[c] == a[r][c],1,0) for c in range(cols)]) == 4)

  # search and result
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xval = [mod.eval(x[j]).as_long() for j in range(cols)]
    print("Account number:")
    for j in range(cols):
      print("%i " % xval[j], end=' ')
    print()
    print("\nThe four tries, where '!' represents a correct digit:")
    for i in range(rows):
      for j in range(cols):
        check = " "
        if a[i][j] == xval[j]:
          check = "!"
        print("%i%s" % (a[i][j], check), end=' ')
      print()
    print()
    getDifferentSolution(sol,mod,x)
    
  print()


  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  main()


