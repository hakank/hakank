"""
Crypto problem in cpmpy.

Prolog benchmark problem GNU Prolog (crypta.pl)
'''
Name           : crypta.pl
Title          : crypt-arithmetic
Original Source: P. Van Hentenryck's book
Adapted by     : Daniel Diaz - INRIA France
Date           : September 1992

Solve the operation:

  B A I J J A J I I A H F C F E B B J E A
   + D H F G A B C D I D B I F F A G F E J E
   -----------------------------------------
   = G J E G A C D D H F A F J B F I H E E F
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def crypto():

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
  LD = intvar(1,num_letters,shape=num_letters,name="LD")
  A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z = LD

  #
  # constraints
  #
  model = Model([AllDifferent(LD),
                B + A + L + L + E + T == BALLET,
                C + E + L + L + O == CELLO,
                C + O + N + C + E + R + T == CONCERT,
                F + L + U + T + E == FLUTE,
                F + U + G + U + E == FUGUE,
                G + L + E + E == GLEE,
                J + A + Z + Z == JAZZ,
                L + Y + R + E == LYRE,
                O + B + O + E == OBOE,
                O + P + E + R + A == OPERA,
                P + O + L + K + A == POLKA,
                Q + U + A + R + T + E + T == QUARTET,
                S + A + X + O + P + H + O + N + E == SAXOPHONE,
                S + C + A + L + E == SCALE,
                S + O + L + O == SOLO,
                S + O + N + G == SONG,
                S + O + P + R + A + N + O == SOPRANO,
                T + H + E + M + E == THEME,
                V + I + O + L + I + N == VIOLIN,
                W + A + L + T + Z == WALTZ])

  def print_sol():
    str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"    
    for (letter, val) in [(str[i], LD[i].value()) for i in range(num_letters)]:
      print("%s: %i" % (letter, val))
    print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


crypto()
