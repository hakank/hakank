#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# de Bruijn sequences in Z3
#
# Implementation of de Bruijn sequences in Minizinc, both 'classical' and 'arbitrary'.
# The 'arbitrary' version is when the length of the sequence (m here) is < # base**n.
#
# base = 2  # the base to use, i.e. the alphabet 0..n-1
# n    = 3  # number of bits to use (n = 4 -> 0..base^n-1 = 0..2^4 -1, i.e. 0..15)
# m    = base**n  # the length of the sequence. For "arbitrary" de Bruijn sequences

# base = 4
# n    = 4
# m    = base**n

#
# for n = 4 with different value of base
# base = 2  0.030 seconds  16 failures
# base = 3  0.041         108
# base = 4  0.070         384
# base = 5  0.231        1000
# base = 6  0.736        2160
# base = 7  2.2 seconds  4116
# base = 8  6 seconds    7168
# base = 9  16 seconds  11664
# base = 10 42 seconds  18000
#
# This is a harder problem:
#  $ python debruijn_binary.py 13 4 52
# base = 13
# n = 4
# m = 52
#
# Compare with the the web based programs:
# - http://www.hakank.org/comb/debruijn.cgi
# - http://www.hakank.org/comb/debruijn_arb.cgi
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *


# converts a number (s) <-> an array of numbers (t) in the specific base.
# def toNum(sol, t, s, base):
#   tlen = len(t)
#   sol.add(s == Sum([(base**(tlen-i- 1))*t[i] for i in range(tlen)]))

def debruijn(base=2, n=3, m=8, num_sols=0, symm=False,enforce_gcc=False):

  print("base: %i n: %i m: %i" % (base, n, m))
    
  # sol = Solver()
  # sol = SimpleSolver()
  sol = SolverFor("QF_FD")
  # sol = SolverFor("LIA")
  # sol = SolverFor("QF_LIA")


  # declare variables
  # x = Array("x",IntSort(),IntSort())
  # x = IntVector("x",m)
  x = [Int("x%i" % i) for i in range(m)]
  for i in range(m):
      sol.add(x[i]>=0, x[i]<=(base**n)-1)
  
  binary = {}
  for i in range(m):
    for j in range(n):
      binary[(i,j)] = Int("binary_%i_%i"%(i,j))
      sol.add(binary[(i,j)] >= 0, binary[(i,j)] <= base-1)

  bin_code = [Int("bin_code%i" % i) for i in range(m)]
  for i in range(m): sol.add(bin_code[i] >= 0, bin_code[i] <= base-1) 

  #
  # constraints
  #
  sol.add(Distinct([x[i] for i in range(m)]))

  # converts x <-> binary
  tc = 0
  for i in range(m):
    # Note: we must add "_tc" to make these variables unique. (This is a gotcha!)
    t = [Int("t_%i_%i" % (j,tc)) for j in range(n)]
    for j in range(n): sol.add(t[j]>= 0, t[j] <= base-1)
    toNum(sol,t,x[i], base)
    for j in range(n):
      sol.add(binary[(i,j)] == t[j])
    tc += 1

  # the de Bruijn condition
  # the first elements in binary[i] is the same as the last
  # elements in binary[i-i]
  for i in range(1, m):
    for j in range(1, n):
      sol.add(binary[(i-1,j)] == binary[(i,j-1)])

  # ... and around the corner
  for j in range(1, n):
    sol.add(binary[(m-1,j)] == binary[(0,j-1)])

  # converts binary -> bin_code
  for i in range(m):
    sol.add(bin_code[i] == binary[(i,0)])

  #
  # Flag: symm: Symmetry breaking
  #
  if symm == True:
      sol.add(bin_code[0] == 0)

  #
  # Flag: enforce_gcc (global cardinality count)
  # Ensure that all the values in the de Bruijn sequence
  # (bin_code) has the same number of occurrences
  # (if mathematically possible).
  #
  if enforce_gcc and m % base == 0:
    r = m // base # Number of occurrences of each value
    print(f"gcc is enforced. There should be {r} occurrences of each value.")
    gcc = [Int(f"gcc[{i}") for i in range(base)]
    for i in range(base):
      sol.add(gcc[i] == r)
    global_cardinality_count(sol,list(range(base)), bin_code, gcc)

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
      num_solutions += 1
      mod = sol.model()
      xx = [mod.eval(x[i]) for i in range(m)]
      # print("x:", xx,end=" ")
      bin_code_s = [mod.eval(bin_code[i]) for i in range(m)]
      print(" seq:", bin_code_s)
      if num_sols > 0 and num_solutions >= num_sols:
        break
      sol.add(
          Or(
             Or([x[i] != xx[i] for i in range(m)]),
             Or([bin_code[i] != bin_code_s[i] for i in range(m)])
          ))

  print("num_solutions:", num_solutions)


symmetry_breaking = True
enforce_gcc = True
base = 2
n = 3
m = base ** n
num_sols = 0
if __name__ == "__main__":
  if len(sys.argv) > 1:
    base = int(sys.argv[1])
  if len(sys.argv) > 2:
    n = int(sys.argv[2])
  if len(sys.argv) > 3:
    m = int(sys.argv[3])
  else:
    m = base**n
  if len(sys.argv) > 4:
    num_sols = int(sys.argv[4])

  debruijn(base, n, m, num_sols, symmetry_breaking, enforce_gcc) 
