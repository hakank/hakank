#!/usr/bin/python
"""
Map coloring problem in Numberjack.

From Pascal Van Hentenryck "The OPL Optimization Programming Language",
page 7, 42.

Compare with the following models:
* Comet: http://www.hakank.org/comet/map.co

This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/
"""
from Numberjack import *
from Mistral import Solver
# from SCIP import Solver


def map_coloring(libs):
    Belgium     = 0
    Denmark     = 1
    France      = 2
    Germany     = 3
    Netherlands = 4
    Luxembourg  = 5
    
    color = VarArray(6, 1, 4)

    model = Model(
         color[Belgium] == 1, # Symmetry breaking
         color[France] != color[Belgium],
         color[France] != color[Luxembourg],
         color[France] != color[Germany],
         color[Luxembourg] != color[Germany],
         color[Luxembourg] != color[Belgium],
         color[Belgium] != color[Netherlands],
         color[Belgium] != color[Germany],
         color[Germany] != color[Netherlands],
         color[Germany] != color[Denmark]
        )

    print model

    for library in libs:
        solver = model.load(library)
        print ''
        solver.solve()
        solver.printStatistics()
        print "color: ", color
        print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        if library == 'Mistral':
            num_solutions = 1
            while solver.getNextSolution() == SAT:
                print "color: ", color
                num_solutions += 1
            print "number of solutions: ", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
                

# map_coloring(['Mistral','SCIP'])
map_coloring(['Mistral'])
