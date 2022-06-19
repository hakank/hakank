"""
Project Euler problem 31 in cpmpy.
'''
In England the currency is made up of pound, £, and pence, p, and 
there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
'''

This is quite slow. There are much faster ways to solve
this problem. See http://hakank.org/python/euler.py


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort

def euler31():

   coins = [200,100,50,20,10,5,2,1]
   Max = max(coins)
   n = len(coins)
   x = intvar(0,Max,shape=n,name="x")
   model = Model(200 == sum(coins*x))

   ss = CPM_ortools(model)
   ss.ort_solver.parameters.linearization_level = 0
   ss.ort_solver.parameters.cp_model_probing_level = 0
   
   num_solutions = ss.solveAll(x)
   print("Nr solutions:", num_solutions)

euler31()
