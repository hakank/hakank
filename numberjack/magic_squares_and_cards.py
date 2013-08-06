#!/usr/bin/python
"""
Magic squares and cards problem in Numberjack.

Martin Gardner (July 1971)
'''
Allowing duplicates values, what is the largest constant sum for an order-3
magic square that can be formed with nine cards from the deck.
'''

This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

# All occurrences in an array
# Syntax:
#  counts = VarArray(n,0,n,'counts')
#  # ....
#  Model(
#     Occurrences(array, counts)
#  )
#
# Note: The built in Gcc(array, gcc_array) uses a
# "fixed" gcc_array.
#
class Occurrences(Predicate):  

     def __init__(self, vars, counts, lb2, ub2):
         Expression.__init__(self, "Occurrences")
         self.set_children(vars)
         self.counts = counts
         #self.lb2 = counts[0].lb # lower bound
         #self.ub2 = counts[0].ub # upper bound
         self.lb2 = lb2 # lower bound
         self.ub2 = ub2 # upper bound

     def decompose(self):
         return [ self.counts[c] == Cardinality(self.children, c) for c in range(self.lb2,self.ub2+1) ]


#
# Decomposition of atmost().
# But in this model I prefer the explicit version using
# Occurrence since we can see the occurrences.
#
def atmost(self, max_val, lb, ub):
    counts = VarArray(ub+1, 0, max_val,'counts')
    decompose = (
        Occurrences(self, counts,lb,ub)
        )    
    return decompose

def model(libs):

    n = 3
    x = Matrix(n, n, 1, 13, 'x')
    s = Variable(0, 13*4, 's')

    # Atmost 4
    # For Gcc (but it don't work)
    gcc_card = dict([(i,(0,4)) for i in range(1,13+1)])

    # count of each "card" (atmost 4)
    counts = VarArray(14, 0, 4,'counts')
    
    model = Model (
        Maximize(s),

        # there are 4 cards of each value in a deck
        
        # Note: I could not get Gcc to work here, so
        # I roll my own (Occurrences)
        # Gcc(x.flat, gcc_card),

        # This works as well
        # atmost(x.flat,4,0,13),
        
        Occurrences(x.flat, counts,0,13),
        # This is not needed since the domain in counts is 0..4
        #[val <= 4 for val in counts],
        
        # the standard magic square constraints (sans all_different)
        [ Sum(row) == s for row in x.row],
        [ Sum(col) == s for col in x.col],               
        Sum([ x[i,i] for i in range(n)]) == s, # diag 1
        Sum([ x[i,n-i-1] for i in range(n)]) == s, # diag 2       
        )

    print model

    for library in libs:
        solver = model.load(library) # Load up model into solver
        if solver.solve():
            solver.printStatistics()
            print 'x:\n', x
            print 's:', s
            print "counts:", counts
            num_solutions = 1
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                print 'x:\n', x
                print 's:', s
                print "counts:", counts
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


