#!/usr/bin/python
"""
de Bruijn sequence in Numberjack.

Implementation of de Bruijn sequences in Comet, both "classical" 
and "arbitrary". 

Compare with the the web based programs:
  http://www.hakank.org/comb/debruijn.cgi   
  http://www.hakank.org/comb/debruijn_arb.cgi

And the following in other constraint programming systems
* MiniZinc: http://www.hakank.org/minizinc/debruijn_binary.mzn
* Choco   : http://www.hakank.org/choco/DeBruijn.java
* JaCoP   : http://www.hakank.org/JaCoP/DeBruijn.java
* JaCoP   : http://www.hakank.org/JaCoP/DeBruijnIterate.java
* Gecode/R: http://www.hakank.org/gecode_r/debruijn_binary.rb
* Comet   : http://www.hakank.org/comet/debruijn.co
* Gecode  : http://www.hakank.org/gecode/debruijn.cpp
* ECLiPSe : http://www.hakank.org/eclipse/debruijn.ecl
* Tailor/Essence' : http://www.hakank.org/tailor/debruijn.eprime

Example:
For base = 2, n = 3, m = base^n = 8 there are 2 solutions:
  x       : 0 1 2 5 3 7 6 4 
  bin_code: 0 0 0 1 0 1 1 1 

  x       : 0 1 3 7 6 5 2 4 
  bin_code: 0 0 0 1 1 1 0 1 


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

from Mistral import Solver

#
# Darn, Mistral seems to calculatie x correct, 
# but the binary matrix (and thus bin_code) is
# wrong.
#
# NumberjackSolver and SCIP gets it correct, however.
#
def debruijn(libs, base=2, n=3, m=0):

    if m == 0:
        m = base**n

    print "base:", base, "n:",n, "m:", m

    #
    # Decision variables
    # Note: Matrix is defined with cols,rows,...
    #
    x        = VarArray(m, 0, (base**n)-1)
    binary   = Matrix(m, n, 0, base-1)
    bin_code = VarArray(m, 0, base-1)

    model = Model(
                 AllDiff(x)
        )

    # convert x[i] <-> binary[i,..]
    model.add([x[i] == Sum([binary[i][j]*(base**(n-j-1)) for j in range(n)]) for i in range(m)])

    # de Bruijn property for the nodes
    model.add([binary[i-1][j] == binary[i][j-1] for j in range(1,n) for i in range(1,m)])

    # ... and around the corner
    model.add([binary[m-1][j] == binary[0][j-1] for j in range(1,n)])

    # convert binary -> bin_code
    model.add([bin_code[i] == binary[i][0] for i in range(m)])

    # symmetry breaking: x[1] is the minimum number
    model.add([x[0] < x[i] for i in range(1,m)])


    # test to "touch" everything for the Mistral solver
    model.add([binary[i][j] >=0  for i in range(m) for j in range(n)])
    model.add([x[i] >= 0 for i in range(m)])
    model.add([bin_code[i] >= 0 for i in range(m)])


    for library in libs:
        solver = model.load(library)
        # print solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print "x: ", x
            print "binary:\n", binary
            print "bin_code:", bin_code
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            num_solutions = 1
            while library == 'Mistral' and solver.getNextSolution():
                print "x: ", x
                print "binary:\n", binary
                print "bin_code:", bin_code
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
                num_solutions += 1
            print "number of solutions:", num_solutions
        else:
            print "No solution"
        print ''

#
# Print the mines
#
def print_matrix(x, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print x[i][j].get_value(),
        print ''



base   = 2
n      = 3
m      = base**n
solver = "NumberjackSolver"
if len(sys.argv)>1:
    base = int(sys.argv[1])
if len(sys.argv)>2:
    n = int(sys.argv[2])
if len(sys.argv)>3:
    m = int(sys.argv[3])
else:
    m = base**n
if len(sys.argv)>4:
    solver = sys.argv[4]

#debruijn(['SCIP','NumberjackSolver','Mistral'], base, n, m)
debruijn(['Mistral'], base, n, m)
# debruijn(['SCIP'], base, n, m)
# debruijn(['NumberjackSolver'], base, n, m)
