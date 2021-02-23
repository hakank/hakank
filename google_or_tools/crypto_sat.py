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

  Crypto problem in OR-tools CP-SAT Solver.

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

  This is a port of my old CP model crypto.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/
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
  LD = [model.NewIntVar(1, num_letters, "LD[%i]" % i) for i in range(num_letters)]
  A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z = LD

  #
  # constraints
  #
  model.AddAllDifferent(LD)
  model.Add(B + A + L + L + E + T == BALLET)
  model.Add(C + E + L + L + O == CELLO)
  model.Add(C + O + N + C + E + R + T == CONCERT)
  model.Add(F + L + U + T + E == FLUTE)
  model.Add(F + U + G + U + E == FUGUE)
  model.Add(G + L + E + E == GLEE)
  model.Add(J + A + Z + Z == JAZZ)
  model.Add(L + Y + R + E == LYRE)
  model.Add(O + B + O + E == OBOE)
  model.Add(O + P + E + R + A == OPERA)
  model.Add(P + O + L + K + A == POLKA)
  model.Add(Q + U + A + R + T + E + T == QUARTET)
  model.Add(S + A + X + O + P + H + O + N + E == SAXOPHONE)
  model.Add(S + C + A + L + E == SCALE)
  model.Add(S + O + L + O == SOLO)
  model.Add(S + O + N + G == SONG)
  model.Add(S + O + P + R + A + N + O == SOPRANO)
  model.Add(T + H + E + M + E == THEME)
  model.Add(V + I + O + L + I + N == VIOLIN)
  model.Add(W + A + L + T + Z == WALTZ)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  if status == cp.OPTIMAL:
    for (letter, val) in [(str[i], solver.Value(LD[i])) for i in range(num_letters)]:
      print("%s: %i" % (letter, val))
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
