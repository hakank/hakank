# -*- coding: utf-8 -*-
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
  Simple regular expression in OR-tools CP-SAT Solver.

  My last name (Kjellerstrand) is quite often misspelled
  in ways that this regular expression shows:
    k(je|ä)ll(er|ar)?(st|b)r?an?d

  This model generates all the words that can be construed
  by this regular expression.

  This is a port of my old CP model regexp.py

  This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import regular, regular_table


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n, s, x, res):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__s = s
        self.__x = x
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        # We collect the solutions by adding it to res
        # Note: 1 is the start state which is not included in the
        #       state array (x)
        x_val =  [1] + [self.Value(self.__x[i]) for i in range(self.__n)]
        ss = ''.join([str(self.__s[i-1]) for i in x_val])
        res.append(ss)


    def SolutionCount(self):
        return self.__solution_count


def main(n, res):

    model = cp.CpModel()    

    #
    # data
    #
    # the DFA (for regular)
    n_states = 11
    input_max = 12
    initial_state = 1 # 0 is for the failing state
    accepting_states = [12]

    # The DFA
    transition_fn =  [
       # 1 2 3 4 5 6 7 8 9 0 1 2     # 
        [0,2,3,0,0,0,0,0,0,0,0,0],   #  1 k 
        [0,0,0,4,0,0,0,0,0,0,0,0],   #  2 je
        [0,0,0,4,0,0,0,0,0,0,0,0],   #  3 ä
        [0,0,0,0,5,6,7,8,0,0,0,0],   #  4 ll
        [0,0,0,0,0,0,7,8,0,0,0,0],   #  5 er
        [0,0,0,0,0,0,7,8,0,0,0,0],   #  6 ar
        [0,0,0,0,0,0,0,0,9,10,0,0],  #  7 st 
        [0,0,0,0,0,0,0,0,9,10,0,0],  #  8 b
        [0,0,0,0,0,0,0,0,0,10,0,0],  #  9 r
        [0,0,0,0,0,0,0,0,0,0,11,12], # 10 a
        [0,0,0,0,0,0,0,0,0,0,0,12],  # 11 n
                                     # 12 d 
        ]

    s = ['k','je','ä','ll','er','ar','st','b','r','a','n','d']
    print('n:', n)

    #
    # declare variables
    #

    x = [model.NewIntVar(1, 12, 'x[%i]'% i) for i in range(n)]


    #
    # constraints
    #
    regular_table(model, x, n_states, input_max, transition_fn,
            initial_state, accepting_states)
  
    #
    # solution and search
    #   
    solver = cp.CpSolver()
    solution_printer = SolutionPrinter(n,s,x, res)
    _status = solver.SearchForAllSolutions(model, solution_printer)

    print('NumConflicts:', solver.NumConflicts())
    print('NumBranches:', solver.NumBranches())
    print('wall_time:', solver.WallTime())
    print()


if __name__ == '__main__':
    res = []
    for n in range(4,9+1):
        main(n, res)
    print('The following %i words where generated:' % len(res))
    for r in res:
        print(r)
