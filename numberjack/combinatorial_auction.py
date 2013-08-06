#!/usr/bin/python
"""
Combinatorial auction in Numberjack.

This is from the Tutorial, page 24 (slide 51/175)

Solution:
""
assign:
  obj in {0..100}
  x1 in {0,1}
  x2 in {0,1}
  x3 in {0,1}
  x4 in {0,1}
  x5 in {0,1}
  
subject to:
  Maximise(obj)
  (obj == (((((x1 * 10) + (x2 * 20)) + (x3 * 30)) + (x4 * 40)) + (x5 * 14)))
  (((x1 + x2) + x5) <= 1)
  (((x1 + x3) + x4) <= 1)
  ((x2 + x4) <= 1)
  ((x3 + x4) <= 1)
  


X  :  0 0 0 1 1
obj: 54
""

For a more general model, see
* http://www.hakank.org/numberjack/combinatorial_auction2.py


This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/
"""
from Numberjack import *
from Mistral import Solver

def combinatorial_auction(library):

    x1 = Variable(0,1,'x1');
    x2 = Variable(0,1,'x2');
    x3 = Variable(0,1,'x3');
    x4 = Variable(0,1,'x4');
    x5 = Variable(0,1,'x5');
    
    obj = Variable(0,100,'obj')
    model = Model(
        Maximise(obj),
        obj == 10*x1 + 20*x2 + 30*x3 + 40*x4 + 14*x5,
        x1 + x2 + x5 <= 1,
        x1 + x3 + x4 <= 1,
        x2 + x4 <= 1,
        x3 + x4 <= 1
        )
    
    print model
    solver = model.load(library)
    if solver.solve():
        solver.printStatistics()
        print "X  : ", x1,x2,x3,x4,x5
        print "obj:", obj
        print
        while solver.getNextSolution() == SAT:
            print "X  : ", x1,x2,x3,x4,x5
            print "obj:", obj
            print
    else:
        print "No solution"

combinatorial_auction('Mistral')
