#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Magic sequence problem in Z3
#  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
#  """
#  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
# 0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
# times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
#  since 0 occurs 6 times in it, 1 occurs twice, ...
# """
#
# Time for n=1..19:
#  - just global_cardinality_count or loop of count/4: 9.6s
#  - gcc + Sum: 4.3s
#  - gcc + scalar_product: 1.6s
#  - gcc + Sum + scalar_product: 1.4s
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *
  
def magic_sequence(n):
    sol = Solver()
    x = makeIntVector(sol,"x",n, 0,n)
    
    # This is the only constraint that is really needed
    global_cardinality_count(sol,[i for i in range(n)],x,x)

    # for i in range(n): count(sol,i,x,x[i]) # slower

    # some extras for speed up (see above)
    sol.add(Sum([x[i] for i in range(n)]) == n)
    scalar_product(sol,[i for i in range(n)],x,n)

    if sol.check() == sat:
        mod = sol.model()
        print [mod.eval(x[i]) for i in range(n) ]
    else:
        print "No solution!"

for n in range(1,20):
    print "Testing ", n 
    magic_sequence(n)
    print


