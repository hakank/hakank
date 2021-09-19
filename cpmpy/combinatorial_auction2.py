"""
Combinatorial auction in cpmpy.

This is a more general model for the combinatorial example
from the Numberjack Tutorial, pages 9 and 24 (slides  19/175 and 51/175).

The original and more talkative model is combinatorial_auction.py

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *
from collections import defaultdict


def combinatorial_auction():

    n = 5 # number of bids
    bid_amount = [10,20,30,40,14]
    # the items for each bid
    items = [
           [0,1],   # A,B
           [0,2],   # A, C
           [1,3],   # B,D
           [1,2,3], # B,C,D
           [0]      # A
           ]
    
    x = intvar(0,1,shape=n, name="x")

    # collect the bids for each item
    items_t = defaultdict(list)
    [items_t[j].append(i) for i in range(n) for j in items[i] ]
    

    obj = intvar(0,100,name='obj')    
    model = Model([
                   obj == sum(x*bid_amount),
                   [ [sum( [x[bid] for bid in items_t[item]] ) <= 1]  for item in items_t]
                   ],
                  maximize=obj,
                  )

    # print(model)
    ss = CPM_ortools(model)
    if ss.solve():
        print("x  : ", x.value())
        print("obj: ", obj.value())
        print()
        print("Use the following bids:")
        for bid in range(n):
            if x[bid].value():
                print("bid #", bid, ": ", ['ABCD'[i] for i in items[bid]])
        
    else:
        print("No solution")

combinatorial_auction()

