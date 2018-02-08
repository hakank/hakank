#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Crypto problem in Z3
#
# This is the standard benchmark "crypto" problem.
#
# From GLPK:s model cryto.mod.
#
# """
#    This problem comes from the newsgroup rec.puzzle.
#    The numbers from 1 to 26 are assigned to the letters of the alphabet.
#    The numbers beside each word are the total of the values assigned to
#    the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
#    5, 9, 20 and 13, or any other combination that add up to 47).
#    Find the value of each letter under the equations:
#
#    BALLET  45     GLEE  66     POLKA      59     SONG     61
#    CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
#    CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
#    FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
#    FUGUE   50     OPERA 65     SOLO       37     WALTZ    34
#
#    Solution:
#    A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
#    5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18
#
#    Reference:
#    Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
#    Simple problems, the crypto-arithmetic puzzle ALPHACIPHER. */
# """

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
  num_letters = 26

  BALLET = 45
  CELLO = 43
  CONCERT = 74
  FLUTE = 30
  FUGUE = 50
  GLEE = 66
  JAZZ = 58
  LYRE = 47
  OBOE = 53
  OPERA = 65
  POLKA = 59
  QUARTET = 50
  SAXOPHONE = 134
  SCALE = 51
  SOLO = 37
  SONG = 61
  SOPRANO = 82
  THEME = 72
  VIOLIN = 100
  WALTZ = 34

  #
  # variables
  #
  LD = [makeIntVar(sol, "LD[%i]" % i, 1, num_letters) for i in range(num_letters)]
  A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z = LD

  #
  # constraints
  #
  sol.add(Distinct(LD))
  sol.add(B + A + L + L + E + T == BALLET)
  sol.add(C + E + L + L + O == CELLO)
  sol.add(C + O + N + C + E + R + T == CONCERT)
  sol.add(F + L + U + T + E == FLUTE)
  sol.add(F + U + G + U + E == FUGUE)
  sol.add(G + L + E + E == GLEE)
  sol.add(J + A + Z + Z == JAZZ)
  sol.add(L + Y + R + E == LYRE)
  sol.add(O + B + O + E == OBOE)
  sol.add(O + P + E + R + A == OPERA)
  sol.add(P + O + L + K + A == POLKA)
  sol.add(Q + U + A + R + T + E + T == QUARTET)
  sol.add(S + A + X + O + P + H + O + N + E == SAXOPHONE)
  sol.add(S + C + A + L + E == SCALE)
  sol.add(S + O + L + O == SOLO)
  sol.add(S + O + N + G == SONG)
  sol.add(S + O + P + R + A + N + O == SOPRANO)
  sol.add(T + H + E + M + E == THEME)
  sol.add(V + I + O + L + I + N == VIOLIN)
  sol.add(W + A + L + T + Z == WALTZ)

  num_solutions = 0
  str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for (letter, val) in [(str[i], mod.eval(LD[i]).as_long()) for i in range(num_letters)]:
      print("%s: %i" % (letter, val))
    print()
    getDifferentSolution(sol,mod,LD)

  print()
  print("num_solutions:", num_solutions)


if __name__ == "__main__":
  main()
