#!/usr/bin/python
"""
Knapsack problem in Numberjack.
 
Simple knapsack problem.

This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/
"""
from Numberjack import *
from Mistral import Solver
# from SCIP import Solver


#
# Somewhat general definition of knapsack.
#
def knapsack(values, weights, n):
    z = Variable(0, 10000)
    x = VarArray(len(values), 0, 1)
    model = Model(
        z >= 0,
        z == Sum(x, values),
        Sum(x, weights) <= n,
        Maximise(z)
        )
    return [model, x, z]


def knapsack_model(libs, values, weights, n):
    
    [model, x, z] = knapsack(values, weights, n)
    for library in libs:
        solver = model.load(library)
        print ''
        if solver.solve():
            print "z: ", z
            print "x: ", x
            solver.printStatistics()
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''

values =  [15, 100, 90, 60, 40, 15, 10,  1, 12, 12, 100]
weights = [ 2,  20, 20, 30, 40, 30, 60, 10, 21, 12,   2]
n = 102

# knapsack_model(['SCIP'], values, weights, n)
knapsack_model(['Mistral'], values, weights, n)
# knapsack_model(['NumberjackSolver'], values, weights, n)
