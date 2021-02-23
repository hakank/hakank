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
  Perfect square sequence in OR-tools CP-SAT Solver.

  From 'Fun with num3ers'
  'Sequence'
  http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
  '''
  If we take the numbers from 1 to 15 
      (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
  and rearrange them in such an order that any two consecutive 
  numbers in the sequence add up to a perfect square, we get,
  
  8     1     15     10     6     3     13     12      4      5     11     14        2      7      9
      9    16    25     16     9     16     25     16     9     16     25     16       9     16
  
  
  I ask the readers the following:
  
  Can you take the numbers from 1 to 25 to produce such an arrangement?
  How about the numbers from 1 to 100?
  '''

  Via http://wildaboutmath.com/2010/11/26/wild-about-math-bloggers-111910

  This model is a port of my CP model perfect_square_sequence.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import memberOf


def main(n=15, print_solutions=True, show_num_sols=0):

    sys.stdout.flush()

    model = cp.CpModel()    

    #
    # data
    #
    print('n: ', n)

    # table of possible squares
    squares = [i*i for i in range(1, int(math.sqrt(n*n)))]

    # declare variables
    x = [model.NewIntVar(1, n, 'x[%i]'%i) for i in range(n)]

    #
    # constraints
    #    
    model.AddAllDifferent(x)
    for i in range(1, n):
        memberOf(model,squares,x[i-1]+x[i])

    # symmetry breaking
    model.Add(x[0] < x[n-1])

    #
    # solution and search
    #
    solver = cp.CpSolver()

    # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
    # solver.parameters.cp_model_presolve = False
    # solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

    status = solver.Solve(model)
    
    if status == cp.OPTIMAL:
        if print_solutions:
            x_val = [solver.Value(x[i]) for i in range(n)]
            print('x: ', x_val)
            # for i in range(1, n):
            #    print((x_val[i-1]+x_val[i]),end=" ")
            # print()
    
    print()
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("wall_time:", solver.WallTime())
    print()


n = 15
if __name__ == '__main__':
    if len(sys.argv) > 1:
        n = int(sys.argv[1])

    if n == 0:
        for i in range(1,100):
           main(i, True, 0)
    else:
        main(n, True, 0)
