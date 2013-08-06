#!/usr/bin/python
"""
To num test in Numberjack.

Test of converting an array of digits <-> number
given a base.

This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

from Mistral import Solver
# from SCIP import Solver


def to_num(x, num, base, model):
    n = len(x)
    model.add([
        num > 0,
        num == Sum([x[i]*(base**(n-1-i)) for i in range(0,n)])
        ])

def to_num_test(libs):

    num = Variable(1,99999)
    base = 10
    n = 5
    x = VarArray(n, 0, base-1)

    model = Model(
           num == 123,
           # (x[i] == (1,2,3,4,5)[i] for i in range(n)) # the other way: fix the array
           
        )
    to_num(x, num, base, model)
   
    for library in libs:
        solver = model.load(library)
        print ''
        if solver.solve():
            solver.printStatistics()
            print "base: ", base, " num: ", num, 
            print "x: ", x
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            while library == 'Mistral' and solver.getNextSolution():
                print "base: ", base, " num: ", num,                 
                print "x: ", x
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
                
        else:
            print "No solution"
        print ''

# to_num_test(('Mistral','SCIP'))
to_num_test(['Mistral'])
