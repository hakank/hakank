#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# factoroal in Z3
#
# It would be nice to have a bidirectional factorial...
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3 import *

def factorial(sol, n, x):  
   sol.add(x == Product([i for i in range(1,n+1)]))
      
      

x = Int("x")
n = Int("n")

sol = Solver()
# This don't work
# factorial(sol, n, x)
# sol.add(x == 3628800)

# This work (as expected)
factorial(sol, 100, x)
print sol
