# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""

  Secret Santa problem II in OR-tools CP-SAT Solver.

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

  This is a port of my old CP model secret_santa2.py


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *

def main(singe=0):

  model = cp.CpModel()

  #
  # data
  #

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
  santas = [model.NewIntVar(0, n - 1, 'santas[%i]' % i) for i in range(n)]
  santa_distance = [
      model.NewIntVar(0, M, 'santa_distance[%i]' % i) for i in range(n)
  ]

  # total of 'distance', to maximize
  z = model.NewIntVar(0, n * n * n, 'z')

  #
  # constraints
  #
  model.AddAllDifferent(santas)

  model.Add(z == sum(santa_distance))

  # Can't be one own's Secret Santa
  # (i.e. ensure that there are no fix-point in the array.)
  for i in range(n):
    model.Add(santas[i] != i)

  # no Santa for a spouses
  for i in range(n):
    if spouses[i] > -1:
      model.Add(santas[i] != spouses[i])

  # optimize 'distance' to earlier rounds:
  for i in range(n):
    # solver.Add(santa_distance[i] == solver.Element(rounds[i], santas[i]))
    model.AddElement(santas[i],rounds[i],santa_distance[i])



  # cannot be a Secret Santa for the same person
  # two years in a row.
  for i in range(n):
    for j in range(n):
      if rounds[i][j] == 1:
        model.Add(santas[i] != j)

  # objective
  model.Maximize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:

    print('total distances:', solver.Value(z))
    print('santas:', [solver.Value(santas[i]) for i in range(n)])
    for i in range(n):
      print('%s\tis a Santa to %s (distance %i)' % \
            (persons[i],
             persons[solver.Value(santas[i])],
             solver.Value(santa_distance[i])))
    # print 'distance:', [solver.Value(santa_distance[i])
    #                     for i in range(n)]
    print()

  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


single = 0
if __name__ == '__main__':
  print('Secret Santas without single')
  main(single)
  print('\nSecret Santas with single:')
  single = 1
  main(single)
