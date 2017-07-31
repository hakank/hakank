#!/usr/bin/python
"""
Global constraint alldifferent_except_0 in Numberjack.

Decomposition of the global constraint alldifferent except 0.

From Global constraint catalog:
http://www.emn.fr/x-info/sdemasse/gccat/sec4.6.html
'''
Enforce all variables of the collection VARIABLES to take distinct values,
except those variables that are assigned to 0.

Example
 (<5, 0, 1, 9, 0, 3>)

The alldifferent_except_0 constraint holds since all the values
(that are different from 0) 5, 1, 9 and 3 are distinct.
'''

Compare with the following models:
 * Comet: http://www.hakank.org/comet/alldifferent_except_0.co
 * ECLiPSE: http://www.hakank.org/eclipse/alldifferent_except_0.ecl
 * Gecode: http://www.hakank.org/gecode/alldifferent_except_0.cpp
 * Gecode/R: http://www.hakank.org/gecode_r/all_different_except_0.rb
 * MiniZinc: http://www.hakank.org/minizinc/alldifferent_except_0.mzn
 * Choco: http://www.hakank.org/choco/AllDifferentExcept0_test.java
 * JaCoP: http://www.hakank.org/JaCoP/AllDifferentExcept0_test.java
 * Tailor/Essence': http://www.hakank.org/tailor/alldifferent_except_0.eprime


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

#
# This version is inspired by src/Decompose.py
#
# Note that the parameter is Predicate (not Expression)
class AllDiffExcept0(Predicate):     

     def __init__(self, vars):
         Expression.__init__(self, "AllDiffExcept0")
         self.set_children(vars)

     # This version was suggested by Eoin O'Mahony
     def decompose(self):
         return [ ( ((a != 0) & (b != 0)) <= (a != b ) ) for a, b in
                  pair_of(self.children)]


# simple decomposition
def alldifferent_except0(x):
     return [ ( ((a != 0) & (b != 0)) <= (a != b ) ) for a, b in
              pair_of(x)]


class Increasing(Predicate):     
    
    def __init__(self, vars):
        Expression.__init__(self, "Increasing")
        self.set_children(vars)

    def decompose(self):
        x = self.children
        x_len = len(x)
        return [x[i-1] <= x[i] for i in range(1,x_len)]

# Simple decomposition of the increasing constraint
def increasing(x):
    return [x[i-1] <= x[i] for i in range(1,n)]

def a_model(libs, n):
     x = VarArray(n, 0, n,'x')
     z = Variable(0, 10,'z') # Number of zeros
     
     # Just for fun: adds a constraint that there should be
     # less than 3 zeros.
     model = Model(
          # z <= 3,       
          z == Sum([x[i] == 0 for i in range(0,n)]),
          AllDiffExcept0(x),
          # These two versions works well:
          # [( ((a != 0) & (b != 0)) <= (a != b ) ) for a,b in pair_of(x)],
          # alldifferent_except0(x),

          Increasing(x),
          #These two solutions works well:
          #[x[i-1] <= x[i] for i in range(1,n)],
          #increasing(x)
          )


     print model
    
     for library in libs:
          solver = model.load(library) # Load up model into solver
          print ''
          if solver.solve():
               solver.printStatistics()
               num_solutions = 1
               print "z: ", z, " x: ", x
               while solver.getNextSolution() == SAT: 
                    print "z: ", z, " x: ", x
                    num_solutions += 1
               print "number of solutions: ", num_solutions
               print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()                
          else:
               print "No solution"
          print ''


n = 6
# Neither SCIP nor NumberjackSolver works here.
a_model(['Mistral'], n)
