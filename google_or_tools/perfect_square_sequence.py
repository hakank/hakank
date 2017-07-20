# Copyright 2010 Hakan Kjellerstrand hakank@gmail.com
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

  Perfect square sequence in Google CP Solver.

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

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/perfect_square_sequence.mzn

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see http://www.hakank.org/google_or_tools/
  
"""
import sys
import math

from ortools.constraint_solver import pywrapcp

def is_square(v):
    solver = v.solver()
    ub = int(math.sqrt(v.Max()))
    z = solver.IntVar(1, ub, 'z')
    solver.Add(z*z == v)

def main(n=15, print_solutions=True, show_num_sols=0):

    sys.stdout.flush()
    
    # Create the solver.
    solver = pywrapcp.Solver('17x17 challenge')

    #
    # data
    #
    print 'n: ', n

    # create the table of possible squares
    squares = []
    for i in range(1, int(math.sqrt(n*n))):
        squares.append(i*i)
    # print "squares:", squares, len(squares)

    # print 'valid squares:', squares

    # declare variables
    x = [solver.IntVar(1, n, 'x[%i]'%i) for i in range(n)]

    #
    # constraints
    #
    
    solver.Add(solver.AllDifferent(x, True))
    for i in range(1, n):
        # solver.Add(solver.Square(x[i-1]+x[i]))
        # is_square(x[i-1]+x[i])
        solver.Add(solver.MemberCt(x[i-1]+x[i], squares))


    # symmetry breaking
    solver.Add(x[0] < x[n-1])

    #
    # solution and search
    #
    db = solver.Phase(x,
                      # solver.CHOOSE_FIRST_UNBOUND,
                      solver.INT_VAR_DEFAULT,                      
                      solver.INT_VALUE_DEFAULT)

    solver.NewSearch(db)
    
    num_solutions = 0
    while solver.NextSolution():
        num_solutions += 1
        if print_solutions:
            x_val = [x[i].Value() for i in range(n)]
            print 'x: ', x_val
            # for i in range(1, n):
            #    print (x_val[i-1]+x_val[i]),
            # print
        if show_num_sols > 0 and num_solutions >= show_num_sols:
            break

    solver.EndSearch()
    
    print
    print "num_solutions:", num_solutions
    print
    if print_solutions:
        print "failures:", solver.Failures()
        print "branches:", solver.Branches()
        print "wall_time:", solver.WallTime()

    return num_solutions

n = 25
if __name__ == '__main__':
    if len(sys.argv) > 1:
        n = int(sys.argv[1])

    if n == 0:
        sols = []
        for i in range(1,100):
           num = main(i, False, 0)
           sols.append((i,num))
        print sols
    else:
        main(n, True, 0)
