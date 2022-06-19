"""
Cryptarithmetic puzzle in cpmpy.

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



def crypta():

  model = Model()
  
  # variables
  LD = intvar(0,9,shape=10,name="LD")
  A, B, C, D, E, F, G, H, I, J = LD

  Sr1 = intvar(0,1, name="Sr1")
  Sr2 = intvar(0,1, name="Sr2")

  #
  # constraints
  #
  model += [AllDifferent(LD)]
  model += [B >= 1]
  model += [D >= 1]
  model += [G >= 1]

  model += [A + 10 * E + 100 * J + 1000 * B + 10000 * B + 100000 * E +
             1000000 * F + E + 10 * J + 100 * E + 1000 * F + 10000 * G +
             100000 * A + 1000000 * F == F + 10 * E + 100 * E + 1000 * H +
             10000 * I + 100000 * F + 1000000 * B + 10000000 * Sr1]

  model += [C + 10 * F + 100 * H + 1000 * A + 10000 * I + 100000 * I +
             1000000 * J + F + 10 * I + 100 * B + 1000 * D + 10000 * I +
             100000 * D + 1000000 * C + Sr1 == J + 10 * F + 100 * A + 1000 * F +
             10000 * H + 100000 * D + 1000000 * D + 10000000 * Sr2]

  model += [A + 10 * J + 100 * J + 1000 * I + 10000 * A + 100000 * B + B +
             10 * A + 100 * G + 1000 * F + 10000 * H + 100000 * D + Sr2 == C +
             10 * A + 100 * G + 1000 * E + 10000 * J + 100000 * G]

  str = "ABCDEFGHIJ"
  def print_sol():
    for (letter, val) in [(str[i], LD[i].value()) for i in range(len(LD))]:
      print("%s: %i" % (letter, val))
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

crypta()
