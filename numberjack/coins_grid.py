#!/usr/bin/python
"""
Coins grid problem in Numberjack.
  
Problem from 
Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
'''
In a quadratic grid (or a larger chessboard) with 31x31 cells, one should
 place coins in such a way that the following conditions are fulfilled:
   1. In each row exactly 14 coins must be placed.
   2. In each column exactly 14 coins must be placed.
   3. The sum of the quadratic horizontal distance from the main 
      diagonal of all cells containing a coin must be as 
      small as possible.
   4. In each cell at most one coin can be placed.

The description says to place 14x31 = 434 coins on the chessboard 
each row containing 14
coins and each column also containing 14 coins.
'''

Compare with the following models:
* MiniZinc: http://www.hakank.org/minizinc/coins_grid.mzn
* Gecode/R: http://www.hakank.org/gecode_r/coins_grid.rb
* JaCoP: http://www.hakank.org/JaCoP/CoinsGrid.java
* Choco: http:// www.hakank.org/choco/CoinsGrid.java
* Comet: http:// www.hakank.org/comet/coins_grid.co
* Tailor: http://www.hakank.org/tailor/coins_grid.eprime

Note: This is a problem where MIP approaches succeeds very well,
      but constraint programming solver is quite slow.


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""

from Numberjack import *
# from Mistral import Solver
# from SCIP import Solver


def coins_grid(libs, n, c):

    x = Matrix(n, n, 0, 1)
    z = Variable(0, 1000000)

    model = Model([
        # every row adds up to c
        [Sum(row) == c for row in x.row],
        # every col adds up to c
        [Sum(col) == c for col in x.col],
        # quadratic horizonal distance
        z == Sum([x[i,j]*abs(i-j)*abs(i-j) for i in range(0,n) for j in range(0,n)]),
        Minimise(z)
        ]
        )

    for library in libs:
        solver = model.load(library) # Load up model into solver
        print 'library:', library
        solver.solve()
        solver.printStatistics()
        print "z: ", z
        print x
        print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        print ''

n = 31
c = 14
# Mistral is slow, and NumberjackSolver is extremely slow...
# for c in range(1,n):
# SCIP is fast, though.
coins_grid(['SCIP'],n, c)
# coins_grid(['Toulbar2'],n, c)
