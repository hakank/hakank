#!/usr/bin/python
"""
  Place number puzzle in Numberjack.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  ""
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  ""

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/place_number.mzn
  * Comet: http://www.hakank.org/comet/place_number_puzzle.co
  * ECLiPSe: http://www.hakank.org/eclipse/place_number_puzzle.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/place_number_puzzle.pl
  * Gecode: http://www.hakank.org/gecode/place_number_puzzle.cpp
  

This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *
# from Mistral import Solver
# from SCIP import Solver

class MyAllDiff(Predicate):
    
    def __init__(self, vars):
        Expression.__init__(self, "MyAllDiff")
        self.set_children(vars)

    def decompose(self):
        return [var1 != var2 for var1, var2 in pair_of(self.children)]



def model(libs):

    m = 32
    graph =  [
        [1,2],
        [1,3],
        [1,4],
        [2,1],
        [2,3],
        [2,5],
        [2,6],
        [3,2],
        [3,4],
        [3,6],
        [3,7],
        [4,1],
        [4,3],
        [4,6],
        [4,7],
        [5,2],
        [5,3],
        [5,6],
        [5,8],
        [6,2],
        [6,3],
        [6,4],
        [6,5],
        [6,7],
        [6,8],
        [7,3],
        [7,4],
        [7,6],
        [7,8],
        [8,5],
        [8,6],
        [8,7]
        ]
    
    n = 8
    x = VarArray(n, 1, n)

    model = Model (
        AllDiff(x), 
        # MyAllDiff(x),
        # Trickery since there is no abs()
        [  (( x[graph[i][0]-1]-x[graph[i][1]-1] ) > 1) |
           (( x[graph[i][1]-1]-x[graph[i][0]-1] ) > 1)
           for i in range(m)],
           # symmetry breaking
           x[0] < x[n-1]
        )

    #print model

    for library in libs:
        print "library:", library
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print x
            num_solutions = 1
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                print x
            print 'Number of solutions: ', num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print 'Number of solutions', num_solutions
        else:
            print 'No solution'
        print ''


model(['Mistral'])


