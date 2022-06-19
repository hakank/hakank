"""
Traffic lights problem in cpmpy.

CSPLib problem 16
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
'''
Specification:
Consider a four way traffic junction with eight traffic lights. Four of the
traffic lights are for the vehicles and can be represented by the variables
V1 to V4 with domains {r,ry,g,y} (for red, red-yellow, green and yellow).

The other four traffic lights are for the pedestrians and can be represented
by the variables P1 to P4 withdomains {r,g}.

The constraints on these variables can be modelled by quaternary constraints
on
(Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples
{(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.

It would be interesting to consider other types of junction (e.g. five roads
intersecting) as well as modelling the evolution over time of the traffic
light sequence.
...

Results
Only 2^2 out of the 2^12 possible assignments are solutions.

  (V1,P1,V2,P2,V3,P3,V4,P4) =
     {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r),
     (y,r,ry,r,y,r,ry,r)}
     [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1,
     2,1)}

The problem has relative few constraints, but each is very tight. Local
propagation appears to be rather ineffective on this problem.

'''

Note: In this model we use only the table constraint.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def traffic_lights():

  model = Model()
  
  # data
  n = 4
  r, ry, g, y = list(range(n))
  lights = ["r", "ry", "g", "y"]

  # The allowed combinations
  allowed = []
  allowed.extend([(r, r, g, g), (ry, r, y, r), (g, g, r, r), (y, r, ry, r)])

  #
  # declare variables
  #
  V = intvar(0,n-1,shape=n,name="V")
  P = intvar(0,n-1,shape=n,name="P")

  #
  # constraints
  #
  for i in range(n):
    for j in range(n):
      if j == (1 + i) % n:
        model += [Table((V[i], P[i], V[j], P[j]), allowed)]

  def print_sol():
    for i in range(n):
      print("%+2s %+2s" % (lights[V[i].value()], lights[P[i].value()]), end=" ")
    print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print()
  print("num_solutions:", num_solutions)


traffic_lights()
