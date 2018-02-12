#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Crossword puzzle in Z3
#
# This is a standard example for constraint logic programming. See e.g.
#
# http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
# '''
# We are to complete the puzzle
#
#     1   2   3   4   5
#   +---+---+---+---+---+       Given the list of words:
# 1 | 1 |   | 2 |   | 3 |             AFT     LASER
#   +---+---+---+---+---+             ALE     LEE
# 2 | # | # |   | # |   |             EEL     LINE
#   +---+---+---+---+---+             HEEL    SAILS
# 3 | # | 4 |   | 5 |   |             HIKE    SHEET
#   +---+---+---+---+---+             HOSES   STEER
# 4 | 6 | # | 7 |   |   |             KEEL    TIE
#   +---+---+---+---+---+             KNOT
# 5 | 8 |   |   |   |   |
#   +---+---+---+---+---+
# 6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
#   +---+---+---+---+---+       puzzle correspond to the words
#                               that will start at those locations.
#  '''
#
#  The model was inspired by Sebastian Brand's Array Constraint cross word
#  example
#  http://www.cs.mu.oz.au/~sbrand/project/ac/
#  http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Solver()

  #
  # data
  #
  alpha = "_abcdefghijklmnopqrstuvwxyz"
  [a,b,c,d,e,f,g,h,i,j,k,l,m,n, o,p,q,r,s,t,u,v,w,z,y,z] = range(1,26+1)
  # a = 1
  # b = 2
  # c = 3
  # d = 4
  # e = 5
  # f = 6
  # g = 7
  # h = 8
  # i = 9
  # j = 10
  # k = 11
  # l = 12
  # m = 13
  # n = 14
  # o = 15
  # p = 16
  # q = 17
  # r = 18
  # s = 19
  # t = 20
  # u = 21
  # v = 22
  # w = 23
  # x = 24
  # y = 25
  # z = 26

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

  A_flat = makeIntArray(sol,"A_flat",num_words*word_len, 0, 26)
  for i in range(num_words):
     for j in range(word_len):
       sol.add(A_flat[i*word_len + j] == AA[i][j])
  
  E = makeIntArray(sol,"E",n,0,num_words-1) 


  # constraints

  sol.add(Distinct([E[w] for w in range(n)]))

  for I in range(num_overlapping):
    sol.add(A_flat[ E[overlapping[I][0]]*word_len + overlapping[I][1]]
            == A_flat[E[overlapping[I][2]]*word_len + overlapping[I][3]])

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    # print(['E%i(%i)' % (w,mod.eval(E[w]).as_long()) for w in range(n)])
    print_solution(mod, A_flat, E, alpha, n, word_len)
    getDifferentSolution(sol,mod,[E[w] for w in range(n)])

  print()
  print("num_solutions:", num_solutions)


def print_solution(mod, A_flat, E, alpha, n, word_len):
  for ee in range(n):
    print("%i: (%2i)" % (ee, mod.eval(E[ee]).as_long()), end=' ')
    print("".join(["%s" % (alpha[mod.eval(A_flat[ee*word_len+ii]).as_long()]) for ii in range(word_len)]))


if __name__ == "__main__":
  main()

