#!/usr/bin/python
"""
Combinatorial auction in Numberjack.

This is a more general model for the combinatorial example
from the Tutorial, pages 9 and 24 (slides  19/175 and 51/175).

The original and more talkative model is here:
http://www.hakank.org/numberjack/combinatorial_auction.py

This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/
"""
from collections import *
from Numberjack import *

def combinatorial_auction(library):

    N = 5
    X = VarArray(N, ["x" + str(i+1) for i in range(N)])
    print "X:", X
    # the items for each bid
    items = [
           [0,1],   # A,B
           [0,2],   # A, C
           [1,3],   # B,D
           [1,2,3], # B,C,D
           [0]      # A
           ]
    # collect the bids for each item
    items_t = defaultdict(list)
    
    # [items_t.setdefault(j,[]).append(i) for i in range(N) for j in items[i] ]
    # nicer:
    [items_t[j].append(i) for i in range(N) for j in items[i] ]
    
    bid_amount = [10,20,30,40,14]
    obj = Variable(0,100,'obj')    
    model = Model(
        Maximise(obj),
        obj == Sum(X,bid_amount),
        [ [Sum( [X[bid] for bid in items_t[item]] ) <= 1]  for item in items_t]
        )

    print model
    solver = model.load(library)
    # solver.setVerbosity(1)
    if solver.solve():
        print "X  : ", X
        print "obj: ", obj
        print
        print "Use the following bids:"
        for bid in range(N):
            if X[bid].get_value():
                print "bid #", bid, ": ", ['ABCD'[i] for i in items[bid]]

        print '\nNodes:', solver.getNodes(), ' Time:', solver.getTime()
        
    else:
        print "No solution"

combinatorial_auction('Mistral')
# combinatorial_auction('SCIP')
