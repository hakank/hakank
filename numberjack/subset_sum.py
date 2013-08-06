#!/usr/bin/python
"""
Subset sum problem in Numberjack.

From Katta G. Murty: "Optimization Models for Decision Making", page 340
http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
'''
Example 7.8.1

A bank van had several bags of coins, each containing either
16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
street, thieves stole some bags. A total of 100 coins were lost.
It is required to find how many bags were stolen.
'''

Compare with the following models:
* Comet: http://www.hakank.org/comet/subset_sum.co
* ECLiPSE: http://www.hakank.org/eclipse/subset_sum.ecl
* Gecode: http://www.hakank.org/gecode/subset_sum.cpp
* MiniZinc: http://www.hakank.org/minizinc/subset_sum.mzn
* Tailor/Essence': http://www.hakank.org/tailor/subset_sum.py


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

from Mistral import Solver
# from SCIP import Solver


def subset_sum(values, total):
    n = len(values)
    x = VarArray(n, 0, n)
    ss = Variable(0, n)

    model = Model(
                ss == Sum(x),
                total == Sum(x, values)
        )

    return model, x, ss

#
# Solve the Minesweeper problem
#
def subset_sum_model(libs, coins, total):

    model, x, ss = subset_sum(coins, total)
      
    for library in libs:
        solver = model.load(library)
        print 'library:',library
        if solver.solve():
            solver.printStatistics()
            print "ss: ", ss
            print "x: ", x
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            while library == 'Mistral' and solver.getNextSolution():
                print "ss: ", ss
                print "x: ", x
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
                
        else:
            print "No solution"
        print ''


coins = [16, 17, 23, 24, 39, 40]
total = 100
# subset_sum_model(['Mistral', 'SCIP'],coins, total)
subset_sum_model(['Mistral'],coins, total)
