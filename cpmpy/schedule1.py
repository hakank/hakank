"""
Scheduling in cpmpy.

Example from SICStus Prolog:
http://www.sics.se/sicstus/docs/latest/html/sicstus/Cumulative-Scheduling.html#Cumulative%20Scheduling
'''
Cumulative Scheduling

This example is a very small scheduling problem. We consider seven
tasks where each task has a fixed duration and a fixed amount of used
resource:

Task Duration Resource
  t1    16       2
  t2     6       9
  t3    13       3
  t4     7       7
  t5     5      10
  t6    18       1
  t7     4      11

The goal is to find a schedule that minimizes the completion time for
the schedule while not exceeding the capacity 13 of the resource. The
resource constraint is succinctly captured by a cumulative/4
constraint. Branch-and-bound search is used to find the minimal
completion time. 

This example was adapted from [Beldiceanu & Contejean 94]. 
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def scheduling1():

  duration = [16, 6,13, 7, 5,18, 4]
  resource = [ 2, 9, 3, 7,10, 1,11]
  capacity = 13
  
  n = len(duration)
  starts = intvar(1,30,shape=n,name="start")
  ends = intvar(1,50,shape=n,name="end")

  model = Model(minimize=max(ends))
  
  model += [ends[i] == starts[i]+duration[i] for i in range(n)]
  # model += [ends == starts+duration] # Why don't this work?
  
  model += [my_cumulative(starts, duration, resource, capacity)]


  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    print("starts  :", starts.value())
    print("duration:", duration)
    print("resource:", resource)    
    print("ends    :", ends.value())
    print("max(ends):",max(ends.value()))
    print()

scheduling1()
