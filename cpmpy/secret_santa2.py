"""
Secret Santa problem II in cpmpy.

From Maple Primes: 'Secret Santa Graph Theory'
http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
'''
Every year my extended family does a 'secret santa' gift exchange.
Each person draws another person at random and then gets a gift for
them. At first, none of my siblings were married, and so the draw was
completely random. Then, as people got married, we added the restriction
that spouses should not draw each others names. This restriction meant
that we moved from using slips of paper on a hat to using a simple
computer program to choose names. Then people began to complain when
they would get the same person two years in a row, so the program was
modified to keep some history and avoid giving anyone a name in their
recent history. This year, not everyone was participating, and so after
removing names, and limiting the number of exclusions to four per person,
I had data something like this:

Name: Spouse, Recent Picks

Noah: Ava. Ella, Evan, Ryan, John
Ava: Noah, Evan, Mia, John, Ryan
Ryan: Mia, Ella, Ava, Lily, Evan
Mia: Ryan, Ava, Ella, Lily, Evan
Ella: John, Lily, Evan, Mia, Ava
John: Ella, Noah, Lily, Ryan, Ava
Lily: Evan, John, Mia, Ava, Ella
Evan: Lily, Mia, John, Ryan, Noah
'''

Note: I interpret this as the following three constraints:
  1) One cannot be a Secret Santa of one's spouse
  2) One cannot be a Secret Santa for somebody two years in a row
  3) Optimization: maximize the time since the last time

This model also handle single persons, something the original
problem don't mention.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def secret_santa2(singe=0):
 
  #
  # The matrix version of earlier rounds.
  # M means that no earlier Santa has been assigned.
  # Note: Ryan and Mia has the same recipient for years 3 and 4,
  #       and Ella and John has for year 4.
  #       This seems to be caused by modification of
  #       original data.
  #
  n_no_single = 8
  M = n_no_single + 1
  rounds_no_single = [
      # N  A  R  M  El J  L  Ev
      [0, M, 3, M, 1, 4, M, 2],  # Noah
      [M, 0, 4, 2, M, 3, M, 1],  # Ava
      [M, 2, 0, M, 1, M, 3, 4],  # Ryan
      [M, 1, M, 0, 2, M, 3, 4],  # Mia
      [M, 4, M, 3, 0, M, 1, 2],  # Ella
      [1, 4, 3, M, M, 0, 2, M],  # John
      [M, 3, M, 2, 4, 1, 0, M],  # Lily
      [4, M, 3, 1, M, 2, M, 0]  # Evan
  ]

  #
  # Rounds with a single person (fake data)
  #
  n_with_single = 9
  M = n_with_single + 1
  rounds_single = [
      # N  A  R  M  El J  L  Ev S
      [0, M, 3, M, 1, 4, M, 2, 2],  # Noah
      [M, 0, 4, 2, M, 3, M, 1, 1],  # Ava
      [M, 2, 0, M, 1, M, 3, 4, 4],  # Ryan
      [M, 1, M, 0, 2, M, 3, 4, 3],  # Mia
      [M, 4, M, 3, 0, M, 1, 2, M],  # Ella
      [1, 4, 3, M, M, 0, 2, M, M],  # John
      [M, 3, M, 2, 4, 1, 0, M, M],  # Lily
      [4, M, 3, 1, M, 2, M, 0, M],  # Evan
      [1, 2, 3, 4, M, 2, M, M, 0]  # Single
  ]

  if single == 1:
    n = n_with_single
    Noah, Ava, Ryan, Mia, Ella, John, Lily, Evan, Single = list(range(n))
    rounds = rounds_single
  else:
    n = n_no_single
    Noah, Ava, Ryan, Mia, Ella, John, Lily, Evan = list(range(n))
    rounds = rounds_no_single

  M = n + 1

  persons = [
      'Noah', 'Ava', 'Ryan', 'Mia', 'Ella', 'John', 'Lily', 'Evan', 'Single'
  ]

  spouses = [
      Ava,  # Noah
      Noah,  # Ava
      Mia,  # Rya
      Ryan,  # Mia
      John,  # Ella
      Ella,  # John
      Evan,  # Lily
      Lily,  # Evan
      -1  # Single has no spouse
  ]

  #
  # declare variables
  #
  santas = intvar(0,n-1,shape=n,name="santas")
  santa_distance = intvar(0,M,shape=n,name="santa_distance")

  # total of 'distance', to maximize
  z = intvar(0, n*n*n, name="z")

  model = Model(maximize=z)

  #
  # constraints
  #
  model += (AllDifferent(santas))
  model += (z == sum(santa_distance))

  # Can't be one own's Secret Santa
  # (i.e. ensure that there are no fix-point in the array.)
  model += ([santas[i] != i for i in range(n)])

  # no Santa for a spouses
  model += ([santas[i] != spouses[i] for i in range(n) if spouses[i] > -1])

  # optimize 'distance' to earlier rounds:
  for i in range(n):
    # model += (santa_distance[I] == rounds[i,santas[i]]) # Dont work.
    model += (santa_distance[i] == Element(rounds[i], santas[i]))

  # cannot be a Secret Santa for the same person
  # two years in a row.
  for i in range(n):
    for j in range(n):
      if rounds[i][j] == 1:
        model += (santas[i] != j)

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("total distances:", z.value())
    print("santas:", santas.value())
    for i in range(n):
      print("%s\tis a Santa to %s (distance %i)" % \
            (persons[i],
             persons[santas[i].value()],
             santa_distance[i].value()))
    # print("distance:", santa_distance.value())
    print()

  print("num_solutions:", num_solutions)


single = 0
print("Secret Santas without single")
secret_santa2(single)
print("\nSecret Santas with single:")
single = 1
secret_santa2(single)
