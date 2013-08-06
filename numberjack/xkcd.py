#!/usr/bin/python

"""
xkcd problem in Numberjack.

See http://xkcd.com/287/

Some amount (or none) of each dish should be ordered to give a total 
of exact 15.05


Compare with the following models:
* Comet: http://www.hakank.org/comet/xkcd.co
* ECLiPSE: http://www.hakank.org/eclipse/xkcd.ecl
* Tailor/Essence': http://www.hakank.org/tailor/xkcd.eprime
* Gecode: http://www.hakank.org/gecode/xkcd.cpp
* Gecode/R: http://www.hakank.org/gecode_r/xkcd.rb
* MiniZinc: http://www.hakank.org/minizinc/xkcd.mzn


This Numberjack model was written by Hakan Kjellerstrand
(hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""

from Numberjack import *
# As of now, this can not handle getNextSolution
# from NumberjackSolver import Solver
# from SCIP import Solver
from Mistral import Solver


def xkcd(x, z,price):
    model = Model(
        z == Sum(x, price)
        )
    return [model,x]

price = [215, 275, 335, 355, 420, 580]
x = VarArray(len(price),0,100)
z = 1505
[model,x] = xkcd(x, z, price)
print "model:\n", model
solver = Solver(model, x)
print "solver:\n", solver
# solver.setVerbosity(10)
if (solver.solve()):
    # solver.printStatistics() 
    print "x: ", x
    # This don't work in NumberjackSolver, just Mistral 
    while solver.getNextSolution() == SAT:
        # solver.printStatistics() 
        print "x: ", x
    solver.printStatistics() 
else:
   print "No solution"



