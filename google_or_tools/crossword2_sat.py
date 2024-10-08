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

  Crosswords in OR-tools CP-SAT Solver.

  This is a standard example for constraint logic programming. See e.g.

  http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
  '''
  We are to complete the puzzle

     1   2   3   4   5
   +---+---+---+---+---+       Given the list of words:
 1 | 1 |   | 2 |   | 3 |             AFT     LASER
   +---+---+---+---+---+             ALE     LEE
 2 | # | # |   | # |   |             EEL     LINE
   +---+---+---+---+---+             HEEL    SAILS
 3 | # | 4 |   | 5 |   |             HIKE    SHEET
   +---+---+---+---+---+             HOSES   STEER
 4 | 6 | # | 7 |   |   |             KEEL    TIE
   +---+---+---+---+---+             KNOT
 5 | 8 |   |   |   |   |
   +---+---+---+---+---+
 6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
   +---+---+---+---+---+       puzzle correspond to the words
                               that will start at those locations.
  '''

  The model was inspired by Sebastian Brand's Array Constraint cross word
  example
  http://www.cs.mu.oz.au/~sbrand/project/ac/
  http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl


  This is a port of my old CP model crossword2.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main():

  model = cp.CpModel()


  #
  # data
  #
  alpha = "_abcdefghijklmnopqrstuvwxyz"
  a = 1
  b = 2
  c = 3
  d = 4
  e = 5
  f = 6
  g = 7
  h = 8
  i = 9
  j = 10
  k = 11
  l = 12
  m = 13
  n = 14
  o = 15
  p = 16
  q = 17
  r = 18
  s = 19
  t = 20
  u = 21
  v = 22
  w = 23
  x = 24
  y = 25
  z = 26

  num_words = 15
  word_len = 5
  AA = [
      [h, o, s, e, s],  # HOSES
      [l, a, s, e, r],  # LASER
      [s, a, i, l, s],  # SAILS
      [s, h, e, e, t],  # SHEET
      [s, t, e, e, r],  # STEER
      [h, e, e, l, 0],  # HEEL
      [h, i, k, e, 0],  # HIKE
      [k, e, e, l, 0],  # KEEL
      [k, n, o, t, 0],  # KNOT
      [l, i, n, e, 0],  # LINE
      [a, f, t, 0, 0],  # AFT
      [a, l, e, 0, 0],  # ALE
      [e, e, l, 0, 0],  # EEL
      [l, e, e, 0, 0],  # LEE
      [t, i, e, 0, 0]  # TIE
  ]

  num_overlapping = 12
  overlapping = [
      [0, 2, 1, 0],  # s
      [0, 4, 2, 0],  # s
      [3, 1, 1, 2],  # i
      [3, 2, 4, 0],  # k
      [3, 3, 2, 2],  # e
      [6, 0, 1, 3],  # l
      [6, 1, 4, 1],  # e
      [6, 2, 2, 3],  # e
      [7, 0, 5, 1],  # l
      [7, 2, 1, 4],  # s
      [7, 3, 4, 2],  # e
      [7, 4, 2, 4]  # r
  ]

  n = 8

  # declare variables
  A = {}
  for I in range(num_words):
    for J in range(word_len):
      A[(I, J)] = model.NewIntVar(0, 26, "A(%i,%i)" % (I, J))

  A_flat = [A[(I, J)] for I in range(num_words) for J in range(word_len)]
  E = [model.NewIntVar(0, num_words, "E%i" % I) for I in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(E)

  for I in range(num_words):
    for J in range(word_len):
      model.Add(A[(I, J)] == AA[I][J])

  for I in range(num_overlapping):
    # This is what I would do:
    # solver.Add(A[(E[overlapping[I][0]], overlapping[I][1])] ==  A[(E[overlapping[I][2]], overlapping[I][3])])

    # But we must use Element explicitly
    C = model.NewIntVar(0,num_words*word_len,"")
    T1 = model.NewIntVar(0,num_words*word_len,"") 
    model.Add(T1 == E[overlapping[I][0]] * word_len + overlapping[I][1])
    model.AddElement(T1,A_flat, C) 
    T2 = model.NewIntVar(0,num_words*word_len,"") 
    model.Add(T2 == E[overlapping[I][2]] * word_len + overlapping[I][3])
    model.AddElement(T2,A_flat,C)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("E:", [solver.Value(v) for v in E])
    print()
    print_solution(solver, A, E, alpha, n, word_len)


  print()
  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


def print_solution(solver, A, E, alpha, n, word_len):
  for ee in range(n):
    print("%i: (%2i)" % (ee, solver.Value(E[ee])), end=" ")
    print("".join(
        ["%s" % (alpha[solver.Value(A[ee, ii])]) for ii in range(word_len)]))


if __name__ == "__main__":
  main()
