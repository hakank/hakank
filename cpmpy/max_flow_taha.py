"""
Max flow problem in cpmpy.

From Taha 'Introduction to Operations Research', Example 6.4-2

Translated from the AMPL code at
http://taha.ineg.uark.edu/maxflo.txt

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def max_flow_taha():


  #
  # data
  #
  n = 5
  start = 0
  end = n - 1

  nodes = list(range(n))

  # cost matrix
  c = [[0, 20, 30, 10, 0], [0, 0, 40, 0, 30], [0, 0, 0, 10, 20],
       [0, 0, 5, 0, 20], [0, 0, 0, 0, 0]]

  #
  # declare variables
  #
  # We restrict upper bound of x[i,j] below
  x = intvar(0,max(c),shape=(n,n),name="x") 
  out_flow = intvar(0,10000,shape=n,name="out_flow")
  in_flow = intvar(0,10000,shape=n,name="in_flow")  

  total = intvar(0, 10000, name="total")

  model = Model(maximize=total)

  #
  # constraints
  #
  # Restrict upper bound of x[i,j]
  for i in nodes:
    for j in nodes:
      model += (x[i,j] <= c[i][j])
      
  model += (total == sum([x[start, j] for j in nodes if c[start][j] > 0]))

  for i in nodes:
    model += (in_flow[i] == sum([x[j, i] for j in nodes if c[j][i] > 0]))
    model += (out_flow[i] == sum([x[i, j] for j in nodes if c[i][j] > 0]))

  # in_flow == out_flow
  for i in nodes:
    if i != start and i != end:
      model += (out_flow[i] - in_flow[i] == 0)

  s1 = [x[i, start] for i in nodes if c[i][start] > 0]
  if len(s1) > 0:
    model += (sum([x[i, start] for i in nodes if c[i][start] > 0] == 0))

  s2 = [x[end, j] for j in nodes if c[end][j] > 0]
  if len(s2) > 0:
    model += (sum([x[end, j] for j in nodes if c[end][j] > 0]) == 0)

  def print_sol():
    print("total:", total.value())
    print("in_flow:", in_flow.value())
    print("out_flow:", out_flow.value())
    for i in nodes:
      for j in nodes:
        print("%2i" % x[i, j].value(), end=" ")
      print()
    print()


  ss = CPM_ortools(model)
  ss.solveAll(display=print_sol)


max_flow_taha()
