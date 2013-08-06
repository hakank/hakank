#!/usr/bin/python
"""
Assignment problem in Numberjack.
 
Winston 'Operations Research', Assignment Problems, page 393f
(generalized version with added test column)

Compare with the following models:
* Comet   : http://www.hakank.org/comet/assignment.co
* ECLiPSE : http://www.hakank.org/eclipse/assignment.ecl
* Gecode  : http://www.hakank.org/gecode/assignment.cpp
* MiniZinc: http://www.hakank.org/minizinc/assignment.mzn
* Tailor/Essence': http://www.hakank.org/tailor/assignment.eprime


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

#
# Somewhat general definition of assignment
#
def assignment(cost, rows, cols):

    total_cost = Variable(0, 100,'cost')
    x = Matrix(rows, cols, 0, 1) # note: cols, rows
    
    model = Model(
        # total_cost >= 0,
        total_cost ==  Sum([ Sum(x_row, cost_row) for (x_row, cost_row) in zip(x, cost)]),
        
        # exacly one assignment per row, all rows must be assigned   
        [Sum(row) == 1 for row in x.row],
        
        # zero or one assignments per column
        [Sum(col) <= 1 for col in x.col],
        
        Minimise(total_cost)
        )

    return [model, x, total_cost]


def assignment_model(libs, cost, rows, cols):
    
    [model, x, total_cost] = assignment(cost, rows, cols)

    # print model
    
    for library in libs:
        solver = model.load(library)
        print 'library:', library
        
        #if solver.solve():
        solver.solve()
        print "total_cost: ", total_cost
        print "x:\n", x
        solver.printStatistics()
        print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()

# hakank: I added the fifth column to make it more
#         interesting
rows = 4
cols = 5
cost = [[14,  5, 8,  7, 15],
        [ 2, 12, 6,  5,  3],
        [ 7,  8, 3,  9,  7],
        [ 2,  4, 6, 10,  1]
       ]

# assignment_model(['Mistral','SCIP','NumberjackSolver'], cost, rows, cols )
assignment_model(['Mistral'], cost, rows, cols )

