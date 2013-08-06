#!/usr/bin/python
"""
Least diff problem in Numberjack.

The model solves the following problem:
 
 What is the smallest difference between two numbers X - Y
 if you must use all the digits (0..9) exactly once, i.e.
 minimize the difference ABCDE - FGHIJ.

Compare with the following models:
 * MiniZinc: http://www.hakank.org/minizinc/least_diff.mzn
 * Choco   : http://www.hakank.org/choco/LeastDiff2.java
 * JaCoP   : http://www.hakank.org/JaCoP/LeastDiff.java
 * Gecode/R: http://www.hakank.org/gecode_r/least_diff.rb
 * Comet   : http://www.hakank.org/comet/least_diff.co
 * Gecode  : http://www.hakank.org/gecode/least_diff.cpp
 * ECLiPSe : http://www.hakank.org/eclipse/least_diff.ecl
 * Tailor/Essence' : http://www.hakank.org/tailor/leastDiff.eprime

Model created by Hakan Kjellerstrand, hakank@bonetmail.com
See also my Numberjack page: http://www.hakank.org/numberjack/

"""
from Numberjack import *

from Mistral import Solver
# from SCIP import Solver

def least_diff(libs):
    print libs
    a,b,c,d,e,f,g,h,i,j = (Variable(0,9) for val in range(10))
    res                 = Variable(0,200000)
    model = Model(
                  res == (a*10000 + b*1000 + c*100 + d*10 + e) -
                         (f*10000 + g*1000 + h*100 + i*10 + j),
                  a > 0,
                  f > 0,
                  res > 0,
                  AllDiff((a,b,c,d,e,f,g,h,i,j)),
                  Minimise(res)
                  )
    
    for library in libs:
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print "res: ", res
            print a,b,c,d,e, " = ", f,g,h,i,j
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''

least_diff(['Mistral'])
# least_diff(['SCIP'])

