"""
nvalues constraint in cpmpy.

Reference: 
Clobal Constraint Catalog
http://www.emn.fr/x-info/sdemasse/gccat/Cnvalues.html
'''
Purpose
 
  Let N be the number of distinct values assigned to the variables of the 
  VARIABLES collection. Enforce condition N <RELOP> LIMIT to hold.
 
Example
  (<4,5,5,4,1,5>,=,3)
 
The nvalues constraint holds since the number of distinct values occurring within 
the collection 4,5,5,4,1,5 is equal (i.e., RELOP is set to =) to its 
third argument LIMIT=3.
'''

Some solutions:

  x: [3 3 3 3]
  op:1 (<=) val:1 n2:1
  The number of distinct values in x is <= 1 (# distinct values: 1)

  x: [4 3 3 3]
  op:1 (<=) val:2 n2:2
  The number of distinct values in x is <= 2 (# distinct values: 2)

  x: [4 4 3 3]
  op:1 (<=) val:2 n2:2
  The number of distinct values in x is <= 2 (# distinct values: 2)


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations


def print_solution(a):
    x = a[0].value()
    op = a[1][0].value()
    val = a[1][1].value()
    n2 = a[1][2].value()
    print("x:",x)
    relops = ["<","<=","=",">=",">","!="]
    print(f"op:{op} ({relops[op]}) val:{val} n2:{n2}")
    print(f"The number of distinct values in x is {relops[op]} {val} (# distinct values: {n2})")
    print()


def nvalues_test(n):

    relops = ["<","<=","=",">=",">","!="]
    x = intvar(1,n,shape=n,name="x")
    op = intvar(0,len(relops)-1,name="op")
    val = intvar(0,n-1,name="val")
    n2 = intvar(0,n,name="n2") # how many different values?

    model = Model([nvalues(x,op,val),
                   nvalue(n2,x)
                   ])

    ortools_wrapper2(model,[x,[op,val,n2]],print_solution)


n = 4
nvalues_test(n)
