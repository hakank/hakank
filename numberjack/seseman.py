#!/usr/bin/python
"""
Seseman convent problem in Numberjack.

Description of the problem:

n is the length of a border
There are (n-2)^2 "holes", i.e.
there are n^2 - (n-2)^2 variables to find out.

The simplest problem, n = 3 (n x n matrix)
which is represented by the following matrix:

 a b c 
 d   e 
 f g h 

Where the following constraints must hold:

  a + b + c = border_sum
  a + d + f = border_sum
  c + e + h = border_sum
  f + g + h = border_sum
  a + b + c + d + e + f = total_sum

For a (swedish) discussion of this problem, see
"Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
http://www.hakank.org/webblogg/archives/001084.html
and
Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
(using Eclipse code)

It was also is commented in the (swedish) blog post
"Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
http://www.hakank.org/webblogg/archives/001209.html

Also compare with other models:
* MiniZinc: http://www.hakank.org/minizinc/seseman.mzn
* Comet:    http://www.hakank.org/comet/Seseman.co
* JaCoP:    http://www.hakank.org/JaCoP/Seseman.java
* Choco:    http://www.hakank.org/Choco/Seseman.java
* Gecode/R: http://www.hakank.org/gecode_r/seseman.rb
* Tailor/Essence': http://www.hakank.org/tailor/seseman.eprime
* Excel/OpenOffice Scalc: http://www.hakank.org/oocalc_excel/seseman.xls

Model created by Hakan Kjellerstrand, hakank@bonetmail.com
See also my Tailor/Essence page: http://www.hakank.org/minion_tailor

"""
from Numberjack import *

from Mistral import Solver
# from SCIP import Solver

#
# (Check this more:
# When rowsum and totalsum is Variable,
# then it seems that there must be an alternative constraint
# beside the Sum constraints in order to make Mistral
# print them correctly...
# )
#
def seseman(libs):
    a,b,c,d,e,f,g,h = (Variable(0,100) for val in range(8))
    rowsum          = Variable(5,12) # 9
    totalsum        = Variable(18,34) # 24
    model = Model(
                   [
                   rowsum >= 0,
                   totalsum >= 0,
                   a+b+c+d+e+f+g+h == totalsum,
                   a+b+c == rowsum,
                   a+d+f == rowsum,
                   c+e+h == rowsum,
                   f+g+h == rowsum,
                   ]
        )
    print model
    
    for library in libs:
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print ''
            print "rowsum: ", rowsum.get_value(), " totalsum: ", totalsum
            print a, b, c
            print d," ", e
            print f, g, h
            num_solutions = 1
            if library == 'Mistral':
                while solver.getNextSolution():
                    print ''
                    print "rowsum: ", rowsum.get_value(), " totalsum: ", totalsum
                    print a, b, c
                    print d," ", e
                    print f, g, h
                    num_solutions += 1
                    print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print "number solutions: ", num_solutions            
        else:
            print "No solution"
        print ''

# seseman(['SCIP'])
# seseman(['NumberjackSolver'])
seseman(['Mistral'])

