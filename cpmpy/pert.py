"""
Simple PERT model in cpmpy.

From Pascal van Hentenryck 
'Scheduling and Packing In the Constraint Language cc(FD)', page 7f
http://citeseer.ist.psu.edu/300151.html

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def pert():

  # data
  maxTime = 30
  n = 11
  #        a  b  c  d  e  f  g  h  j  k  Send 
  Times = [7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1]
  
  numDependencies = 15
  # Dependencies
  # Note: There is no Si
  # 1-based (fixed below)
  Dependencies = [
    [2,1],  # Sb >= Sa + 7
    [4,1],  # Sd >= Sa + 7
    [3,2],  # Sc >= Sb + 3
    [5,3],  # Se >= Sc + 1
    [5,4],  # Se >= Sd + 8
    [7,3],  # Sg >= Sc + 1
    [7,4],  # Sg >= Sd + 8
    [6,4],  # Sf >= Sd + 8
    [6,3],  # Sf >= Sc + 1
    [8,6],  # Sh >= Sf + 1
    [9,8],  # Sj >= Sh + 3
    [10,7], # Sk >= Sg + 1
    [10,5], # Sk >= Se + 1
    [10,9], # Sk >= Sj + 2
    [11,10] # Send >= Sk + 1
    ]

  # variables
  Start = intvar(0,maxTime,shape=n,name="Start") #  when the activity start
  SumTimes = intvar(0,maxTime*n,name="SumTimes")

  model = Model()
  # model = Model(minimize=SumTimes)
  # model = Model(minimize=sum(Start))
  # model = Model(minimize=Start[-1])

  # constraints
  model += (SumTimes == sum(Start))

  for i in range(numDependencies):
    # also fix for 1-base
    model += (Start[Dependencies[i][0]-1] >= Start[Dependencies[i][1]-1] + Times[Dependencies[i][1]-1])

  model.minimize(sum(Start))

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("SumTimes:",SumTimes.value())
    print("Start:",Start.value())
    print("Times:",Times)
    print("End  :",Start.value() + Times)    

  print("num_solutions:", num_solutions)

pert()
