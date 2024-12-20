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

  3 jugs problem using regular constraint in Google CP-SAT Solver.

  A.k.a. water jugs problem.

  Problem from Taha 'Introduction to Operations Research',
  page 245f .

  For more info about the problem, see:
  http://mathworld.wolfram.com/ThreeJugProblem.html


  This model use a regular constraint for handling the
  transitions between the states. Instead of minimizing
  the cost in a cost matrix (as shortest path problem),
  we here call the model with increasing length of the
  sequence array (x).

  This is a port of my old CP model 3_jugs_regular.py model

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
from cp_sat_utils import regular_element, regular_table
from collections import defaultdict



def main(n):

    model = cp.CpModel()

    #
    # data
    #

    # the DFA (for regular)
    n_states = 14
    input_max = 15
    initial_state = 1  # 0 is for the failing state
    accepting_states = [15]

    ##
    # Manually crafted DFA
    # (from the adjacency matrix used in the other models)
    ##
    transition_fn =  [
       # 1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
        [0, 2, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0],  # 1
        [0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],  # 2
        [0, 0, 0, 4, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0],  # 3
        [0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],  # 4
        [0, 0, 0, 0, 0, 6, 0, 0, 9, 0, 0, 0, 0, 0, 0],  # 5
        [0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0],  # 6
        [0, 0, 0, 0, 0, 0, 0, 8, 9, 0, 0, 0, 0, 0, 0],  # 7
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15], # 8
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0], # 9
        [0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0], # 10
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0], # 11
        [0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0], # 12
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 0], # 13
        [0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15], # 14
                                                        # 15
        ]

    #
    # However, the DFA is easy to create from adjacency lists.
    #
    states = [
        [2, 9],  # state 1
        [3],     # state 2
        [4, 9],  # state 3
        [5],     # state 4
        [6, 9],  # state 5
        [7],     # state 6
        [8, 9],  # state 7
        [15],    # state 8
        [10],    # state 9
        [11],    # state 10
        [12],    # state 11
        [13],    # state 12
        [14],    # state 13
        [15]     # state 14
    ]

    transition_fn = []
    for i in range(n_states):
        row = []
        for j in range(1, input_max + 1):
            if j in states[i]:
                row.append(j)
            else:
                row.append(0)
        transition_fn.append(row)
        
    #
    # The name of the nodes, for printing
    # the solution.
    #
    nodes = [
        '8,0,0',  # 1 start
        '5,0,3',  # 2
        '5,3,0',  # 3
        '2,3,3',  # 4
        '2,5,1',  # 5
        '7,0,1',  # 6
        '7,1,0',  # 7
        '4,1,3',  # 8
        '3,5,0',  # 9
        '3,2,3',  # 10
        '6,2,0',  # 11
        '6,0,2',  # 12
        '1,5,2',  # 13
        '1,4,3',  # 14
        '4,4,0'  # 15 goal
    ]

    #
    # declare variables
    #
    x = [model.NewIntVar(1, input_max, 'x[%i]' % i) for i in range(n)]

    #
    # constraints
    #
    # regular_element(model,x, n_states, input_max, transition_fn, initial_state, 
    #              accepting_states)
    regular_table(model,x, n_states, input_max, transition_fn, initial_state, 
            accepting_states)
            
    #
    # solution and search
    #
    solver = cp.CpSolver()
    status = solver.Solve(model)

    x_val = []
    # num_solutions = 0
    # 
    if status == cp.OPTIMAL:
        x_val = [1] + [solver.Value(x[i]) for i in range(n)]
        print('x:', x_val)
        for i in range(1, n + 1):
            print('%s -> %s' % (nodes[x_val[i - 1] - 1], nodes[x_val[i] - 1]))
    
    # return the solution (or an empty array)
    return x_val


# Search for a minimum solution by increasing
# the length of the state array.
if __name__ == '__main__':
    for n in range(1, 15):
        result = main(n)
        result_len = len(result)
        if result_len > 0:
            print()
            print('Found a solution of length %i:' % result_len, result)
            print()
            break
