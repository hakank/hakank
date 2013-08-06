#!/usr/bin/python
"""
Sicherman Dice in Numberjack.

  From http://en.wikipedia.org/wiki/Sicherman_dice
  "" 
  Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
  bear only positive integers, and have the same probability distribution for 
  the sum as normal dice.
  
  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  ""

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  'Sicherman Dice': http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:
  
  x1 = [1, 2, 3, 4, 5, 6]
  x2 = [1, 2, 3, 4, 5, 6]
  ----------
  x1 = [1, 2, 2, 3, 3, 4]
  x2 = [1, 3, 4, 5, 6, 8]


  Extra: If we also allow 0 (zero) as a valid value then the 
  following two solutions are also valid:
  
  x1 = [0, 1, 1, 2, 2, 3]
  x2 = [2, 4, 5, 6, 7, 9]
  ----------
  x1 = [0, 1, 2, 3, 4, 5]
  x2 = [2, 3, 4, 5, 6, 7]
  
  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html

This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

def model(libs):

    n = 6
    m = 10

    # standard distribution
    standard_dist = [1,2,3,4,5,6,5,4,3,2,1]

    # the two dice
    x1 = VarArray(n, 0, m)
    x2 = VarArray(n, 0, m)

    model = Model (
        [ standard_dist[k] == Sum([x1[i] + x2[j] == k+2 for i in range(n) for j in range(n)])
                                                       for k in range(len(standard_dist))],
        [x1[i] <= x1[i+1] for i in range(n-1)],
        [x2[i] <= x2[i+1] for i in range(n-1)],
        [x1[i] <= x2[i] for i in range(n-1)],                        
        
        )

    # print model

    for library in libs:
        solver = model.load(library)
        solver.setHeuristic('MaxDegree','Lex') 
        if solver.solve():
            solver.printStatistics()
            print "x1:", x1
            print "x2:", x2
            print ''
            num_solutions = 1
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                print "x1:", x1
                print "x2:", x2
                print ''
            print 'Number of solutions: ', num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print 'getPropags:', solver.getPropags()
            print 'getBacktracks:', solver.getBacktracks()
            print 'getFailures:', solver.getFailures()
        else:
            print 'No solution'
        print ''


model(['Mistral'])


