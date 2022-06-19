"""
Number puzzle #8 in cpmpy.

From "Natural Blogarithms"
http://blog.drscottfranklin.net/2009/02/24/number-puzzle-8/
'''
The four numbers A, B, A+B and A-B are all prime.  The sum of these 
four numbers is

A) Even
B) Divisible by 3
C) Divisible by 5
D) Divisible by 7
E) Prime
?

Source: 2002 AMC 10/12B #15
'''

Via 360: 'Math Teachers at Play and some other stuff'
http://threesixty360.wordpress.com/2009/04/17/math-teachers-at-play-and-some-other-stuff/

See https://artofproblemsolving.com/wiki/index.php/2002_AMC_12B_Problems/Problem_11
for discussions about this problem.

Note: This is a question with one answer. 


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def is_prime(n):
  if n < 2: return False
  if n == 2: return True
  if not n & 1:
    return False
  for i in range(3, 1+int(math.sqrt(n)), 2):
    if n % i == 0:
      return False
  return True

def z_is_prime(z,primes):
    return sum([z == p for p in primes]) == 1
    

def number_puzzle8(num_sols=0):

    ub = 100
    primes = [p for p in range(2,ub) if is_prime(p)]

    a = intvar(2,ub,name="a")
    b = intvar(2,ub,name="b")
    a_plus_b = intvar(1,2*ub,name="a_plus_b")
    a_minus_b = intvar(0,ub,name="a_minus_b")
    
    z = intvar(0,4*ub,name="z")

    # z_is_prime = boolvar(name="z_is_prime")
    all_nums = [a,b,a_plus_b,a_minus_b]

    # Then tentative solutions
    sols = boolvar(shape=5,name="sols")
    sols_s = ["even","divisible by 3","divisible by 5","divisible by 7","prime"]

    model = Model([a_plus_b == a+b,
                   a_minus_b == a-b,
                   a > b,
                   [member_of(primes,t) for t in all_nums],
                   z == sum(all_nums),

                   # The alternative solutions 
                   (z % 2 == 0)==(sols[0]),
                   (z % 3 == 0)==(sols[1]),
                   (z % 5 == 0)==(sols[2]),
                   (z % 7 == 0)==(sols[3]),
                   (z_is_prime(z,primes))==(sols[4]),
                   
                   # Exactly one solution
                   sum(sols) == 1,
                   ]
                  )

    def print_sol():
      print("z:",z.value())        
      print("a:", a.value(),"\nb:",b.value(), "\na+b:",a_plus_b.value(),"\na-b:",a_minus_b.value())
      print("sols:",sols.value())
      print("solution: z is",[sols_s[i] for i in range(len(sols)) if sols[i].value()==1][0])
      print()

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print("number of solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    

num_sols = 0
number_puzzle8(num_sols)
