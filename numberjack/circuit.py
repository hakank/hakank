#!/usr/bin/python
"""
Decomposition of the circuit constraint in Numberjack.

Cf Global constraint catalog:
http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html

Solution of n=4:
x: [2, 0, 3, 1]
x: [3, 0, 1, 2]
x: [1, 3, 0, 2]
x: [3, 2, 0, 1]
x: [1, 2, 3, 0]
x: [2, 3, 1, 0]

The 'orbit' method that is used here is based on some
observations on permutation orbits.

Compare with the following models:
* MiniZinc: http://www.hakank.org/minizinc/circuit_test.mzn
* Gecode: http://www.hakank.org/gecode/circuit_orbit.mzn
 

This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

class MyAllDiff(Predicate):
    
    def __init__(self, vars):
        Expression.__init__(self, "MyAllDiff")
        self.set_children(vars)

    def decompose(self):
        return [var1 != var2 for var1, var2 in pair_of(self.children)]


#
# circuit(x)
# constraints x to be an circuit
#
# Note: This assumes that x is has the domain 0..len(x)-1,
#       i.e. 0-based.
#

def circuit(self):
    x = self
    if x[0].lb != 0:
        print "Warning: circuit: lb is", x[0].lb, " must be 0"
        print 
    
    n = len(x)
    z = VarArray(n, 0, n-1, 'z')

    decomp = (
        AllDiff(x),
        AllDiff(z),

        # put the orbit of x[0] in in z[1..n]        
        z[0] == x[0],
        [ z[i] == x[z[i-1]] for i in range(1, n-1)],
        # may not be 0 for i < n-1
        [ z[i] != 0 for i in range(1, n-1)],
        # when i = n-1 it must be 0
        z[n-1] == 0
        )
    
    return decomp
        

    

def model(libs):

    n = 5
    x = VarArray(n, 0, n-1,'x')    
    model = Model (
        circuit(x),
        )

    print model

    for library in libs:
        solver = model.load(library) # Load up model into solver
            
        if solver.solve():
            solver.printStatistics()
            print "x:", x
            num_solutions = 1
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                print "x:", x

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


