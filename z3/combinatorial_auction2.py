#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Combinatorial auction in Z3
#
# This is a more general model for the combinatorial example
# in the Numberjack Tutorial, pages 9 and 24 (slides  19/175 and  51/175).
#
# The original and more talkative model is here:
# http://www.hakank.org/numberjack/combinatorial_auction.py
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from collections import *
from z3_utils_hakank import *



def main():

  sol = SolverFor("QF_FD")

  #
  # data
  #
  N = 5

  # the items for each bid
  items = [
      [0, 1],   # A,B
      [0, 2],   # A, C
      [1, 3],   # B,D
      [1, 2, 3],  # B,C,D
      [0]      # A
  ]
  # collect the bids for each item
  items_t = defaultdict(list)

  # [items_t.setdefault(j,[]).append(i) for i in range(N) for j in items[i] ]
  # nicer:
  [items_t[j].append(i) for i in range(N) for j in items[i]]

  bid_amount = [10, 20, 30, 40, 14]

  #
  # declare variables
  #
  X = [makeIntVar(sol, "x%i" % i, 0, 1) for i in range(N)]
  obj = makeIntVar(sol, "obj", 0, 100)

  #
  # constraints
  #
  sol.add(obj == scalar_product2(sol, X, bid_amount))
  for item in items_t:
    sol.add(Sum([X[bid] for bid in items_t[item]]) <= 1)

  # objective
  # sol.maximize(obj)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("X:", [mod.eval(X[i]) for i in range(N)])
    print("obj:", mod.eval(obj))
    print()
    getGreaterSolution(sol,mod,obj)

  print()
  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  main()
