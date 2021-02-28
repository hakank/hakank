# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""
  
  Number theory problem in 2012 CMO OR-tools CP-SAT Solver.

  http://community.wolfram.com/groups/-/m/t/793922
  '''
  Given two positive integers a and b. The two numbers satisfy the following conditions: 
  a-b is a prime number p and a×b is a perfect square n^2 . 
  Find the smallest value of a no less than 2012.
  '''
    
  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionPrinter2


def is_prime(n):
    if n < 2: return False
    if n == 2: return True
    if not n & 1:
        return False
    for i in range(3, 1+int(math.sqrt(n)), 2):
        if n % i == 0:
          return False
    return True

def primes(limit):
   primes = [2]
   i = 3
   for i in range(3, limit, 2):
      if is_prime(i):
         primes.append(i)
   return primes


def main():

  model = cp.CpModel()

  max_val = 10000
  prime_list = primes(2012)

  a = model.NewIntVar(2012,max_val,"a")
  b = model.NewIntVar(2,2012,"b")
  n = model.NewIntVar(2,2012,"n")
  dom = cp.Domain.FromValues(prime_list)
  p = model.NewIntVarFromDomain(dom,"p")
  
  model.Add(a >= b)
  model.Add(p == a-b)
  # a*b == n*n
  ab_nn = model.NewIntVar(0,2012**2,"a*b, n*n")
  model.AddMultiplicationEquality(ab_nn,[a,b])
  model.AddMultiplicationEquality(ab_nn,[n,n])

  model.Minimize(a)

  solver  = cp.CpSolver()
  status = solver.Solve(model)
  if status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("a:",solver.Value(a), "(the answer of the question)")
    print("b:",solver.Value(b))
    print("n:",solver.Value(n))
    print("p:",solver.Value(p))


  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())

if __name__ == '__main__':
  main()
