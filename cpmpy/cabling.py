"""
Cabling problem in cpmpy

From https://yurichev.com/blog/cabling_Z3/
'''
Take a rack cabinet, like this one:
  [ an image ]
  
Let's say, there are 8 1U devices, maybe servers, routers and whatnot, named
as A, B, C, D, E, F, G, H. Devices must be connected by cables: probably
twisted pair or whatever network engineers using today. Some devices must be
connected by several cables (2 cables, 3 or 4):

A <--- 1 cable  ---> H
A <--- 2 cables ---> E
B <--- 4 cables ---> F
C <--- 1 cable  ---> G
C <--- 1 cable  ---> D
C <--- 1 cable  ---> E
D <--- 3 cables ---> H
G <--- 1 cable  ---> H

The problem: how we can place these 8 devices in such an order, so that sum
of all cable lengths would be as short as possible?
'''

Here are two models:
- cabling1: A port of the original Z3 model
- cabling2: A more general approach.
            This model also shows all the 48 optimal
            solutions (length = 19)

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def diff(x, y):
    return abs(x-y)

# This is a port of the original Z3 model
def cabling_1():
  
  model = Model()
  n = 8
  x = intvar(0,7,shape=n,name="x")
  A, B, C, D, E, F, G, H = x

  final_sum = intvar(-1000,1000,name="final_sum")

  # all "devices" has distinct positions in rack:
  model += [AllDifferent(x)]

  # A <--- 1 cable  ---> H
  diff_A_H = intvar(-7,7,name="diff_A_H")
  model += [diff_A_H == diff(A,H)]
  
  # final_sum = diff_A_H

  # A <--- 2 cables ---> E
  diff_A_E = intvar(-7,7,name="diff_A_E")
  model += [diff_A_E == diff(A,E)]
  # final_sum = final_sum+diff_A_E*2
  
  # B <--- 4 cables ---> F
  diff_B_F = intvar(-7,7,name="diff_B_F")
  model += [diff_B_F == diff(B,F)]
  # final_sum=final_sum+diff_B_F*4
  
  # C <--- 1 cable  ---> G
  diff_C_G = intvar(-7,7,name="diff_C_G")
  model += [diff_C_G == diff(C,G)]
  # final_sum = final_sum+diff_C_G
  
  # C <--- 1 cable  ---> D
  diff_C_D = intvar(-7,7,name="diff_C_D")
  model += [diff_C_D == diff(C,D)]
  # final_sum = final_sum+diff_C_D
  
  # C <--- 1 cable  ---> E
  diff_C_E = intvar(-7,7,name="diff_C_E")
  model += [diff_C_E == diff(C,E)]
  # final_sum = final_sum+diff_C_E
  
  # D <--- 3 cables ---> H
  diff_D_H = intvar(-7,7,name="diff_D_H")
  model += [diff_D_H == diff(D,H)]
  # final_sum = final_sum+diff_D_H*3
  
  # G <--- 1 cable  ---> H
  diff_G_H = intvar(-7,7,name="diff_G_H")
  model += [diff_G_H == diff(G,H)]
  # final_sum = final_sum+diff_G_H

  diffs = cpm_array([diff_A_H, diff_A_E, diff_B_F, diff_C_G, diff_C_D,
           diff_C_E, diff_D_H, diff_G_H])

  model += [final_sum == diff_A_H +
            diff_A_E*2 +
            diff_B_F*4 +
            diff_C_G +
            diff_C_D +
            diff_C_E +
            diff_D_H*3 +
            diff_G_H
            ]
  
  model.minimize(final_sum)

  def print_sol():
    a_s = ["A","B","C","D","E","F","G","H"]
    print("x:", x.value())
    print("diffs:",diffs.value())
    print("final_sum:", final_sum.value())
    order = [a_s[x[i].value()] for i in range(n)]
    print("order:","".join(order))    
    print()
      
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


#
# A more general model
#
def cabling_2(min_val=None):
  print("min_val:", min_val)
  model = Model()

  n = 8
  # A <--- 1 cable  ---> H
  # A <--- 2 cables ---> E
  # B <--- 4 cables ---> F
  # C <--- 1 cable  ---> G
  # C <--- 1 cable  ---> D
  # C <--- 1 cable  ---> E
  # D <--- 3 cables ---> H
  # G <--- 1 cable  ---> H
  A,B,C,D,E,F,G,H = list(range(n))
  cable_struct = [[A,H,1], 
                  [A,E,2],
                  [B,F,4],
                  [C,G,1],
                  [C,D,1],
                  [C,E,1],
                  [D,H,3],
                  [G,H,1]
                  ]

  x = intvar(0,n-1,shape=n,name="x")
  t = intvar(1,n*n,shape=len(cable_struct),name="t")
  final_sum = intvar(0,n*n,name="final_sum")


  # all "devices" has distinct positions in rack:
  model += [AllDifferent(x),
            final_sum == sum(t)]

  for i in range(n):
    a,b,num = cable_struct[i]
    model += [t[i] == abs(x[a]-x[b])*num]

  if min_val == None:
    model.minimize(final_sum)
  else:
    model += [final_sum == min_val]

  def print_sol():
      a_s = ["A","B","C","D","E","F","G","H"]
      print("x:", x.value())
      print("t:",t.value())
      print("final_sum:", final_sum.value())
      order = [a_s[x[i].value()] for i in range(n)]
      print("order:","".join(order))
      print()


  if min_val == None:
    ss = CPM_ortools(model)
    num_solutions = ss.solve()
    print_sol()
    return final_sum.value()
  else:
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)

print("cabling_1:")
cabling_1()

print("\ncabling_2:")
min_val = cabling_2()
print("\nFind all optimal solution:")
cabling_2(min_val)
