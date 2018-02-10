#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint contiguity using regular in Z3
#
# This is a decomposition of the global constraint
# global contiguity.
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
# '''
# Enforce all variables of the VARIABLES collection to be assigned to 0 or 1.
# In addition, all variables assigned to value 1 appear contiguously.
#
# Example:
# (<0, 1, 1, 0>)
#
# The global_contiguity constraint holds since the sequence 0 1 1 0 contains
# no more than one group of contiguous 1.
# '''
#
# This version use the global constraint regular.
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
  # the DFA (for regular)
  n_states = 3
  input_max = 2
  initial_state = 1  # 0 is for the failing state

  # all states are accepting states
  accepting_states = [1, 2, 3]

  # The regular expression 0*1*0*
  transition_fn = [
      [1, 2],  # state 1 (start): input 0 -> state 1, input 1 -> state 2 i.e. 0*
      [3, 2],  # state 2: 1*
      [3, 0],  # state 3: 0*
  ]

  n = 7

  #
  # declare variables
  #

  # We use 1..2 and subtract 1 in the solution
  reg_input = makeIntArray(sol,"reg_input", n, 1,2)

  #
  # constraints
  #
  regular(sol, reg_input, n_states, input_max, transition_fn,
          initial_state, accepting_states, n)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    # Note: here we subract 1 from the solution
    print('reg_input:', [mod.eval(reg_input[i]).as_long()- 1 for i in range(n)])
    getDifferentSolution(sol,mod,[reg_input[i] for i in range(n)])

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
