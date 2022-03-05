#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Sliding sum constraint in Z3
#
# Global constraint sliding_sum in MiniZinc.
# 
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Csliding_sum.html
# """
# sliding_sum(LOW,UP,SEQ,VARIABLES)
# 
# Purpose
#
# Constrains all sequences of SEQ consecutive variables of the collection VARIABLES so that the 
# sum of the variables belongs to interval [LOW, UP].
#
# Example
#     (
#     3, 7, 4,<1, 4, 2, 0, 0, 3, 4>
#     )
#
# The example considers all sliding sequences of SEQ=4 consecutive values of <1, 4, 2, 0,0,3, 4> 
# collection and constraints the sum to be in [LOW,UP] = [3, 7]. The sliding_sum constraint holds 
# since the sum associated with the corresponding subsequences 1 4 2 0, 4 2 0 0, 2 0 0 3, and 
# 0 0 3 4 are respectively 7, 6, 5 and 7. 
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *
import uuid

def sliding_sum(sol, low, up, seq, x):
  vlen = len(x)
  for i in range(vlen-seq+1):
    # s = makeIntVar(sol, "s_%i_%i" % (i,uuid.uuid4().int),low,up)
    s = makeIntVar(sol, "s_%i"%i,low,up)    
    sol.add(s == Sum([x[j] for j in range(i,i+seq)]))
    # sol.add(s >= low, s <= up)

def main():
  sol = SimpleSolver()

  n = 7

  seq = 4
  x = makeIntVector(sol, "x", n, 0,4)
  low = 3
  up = 7
  # low = makeIntVar(sol,"low", 0,10)
  # up = makeIntVar(sol,"up", 0,10)
  
  sliding_sum(sol,low,up,seq,x)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    # print("low:", low)
    # print("up:", up)
    print("x:", [mod.eval(x[i]) for i in range(n)])
    getDifferentSolution(sol,mod,x)

  print("num_solutions:", num_solutions)


if __name__ == "__main__":
  main()
