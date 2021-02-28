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

  Global constraint regular using Automaton in OR-tools CP-SAT Solver.

  Here we test with the following regular expression:
    0*1{3}0+1{2}0+1{1}0*
  using an array of size 10.
  I.e. 111, 11, 1 with some zeroes between.
  This is coded as [3,2,1]

  The automaton is:
  - start state: 0
  - acceptng states: 8 and 9 (the two last states)
  - the transitions:
      (0, 0, 0)  # 0*  start state
      (0, 1, 1)  # 1{3}
      (1, 1, 2)
      (2, 1, 3)
      (3, 0, 4)  # 0+
      (4, 0, 4)
      (4, 1, 5)  # 1{2}
      (5, 1, 6) 
      (6, 0, 7)  # 0+
      (7, 0, 7)
      (7, 1, 8)  # 1{1}
      (8, 0, 9)  # 0* accepting state
      (9, 0, 9)  # 0* accepting states

  There are 10 solutions (length 10):
    [0, 0, 1, 1, 1, 0, 1, 1, 0, 1]
    [0, 1, 1, 1, 0, 0, 1, 1, 0, 1]
    [0, 1, 1, 1, 0, 1, 1, 0, 0, 1]
    [0, 1, 1, 1, 0, 1, 1, 0, 1, 0]
    [1, 1, 1, 0, 0, 0, 1, 1, 0, 1]
    [1, 1, 1, 0, 0, 1, 1, 0, 0, 1]
    [1, 1, 1, 0, 0, 1, 1, 0, 1, 0]
    [1, 1, 1, 0, 1, 1, 0, 0, 0, 1]
    [1, 1, 1, 0, 1, 1, 0, 0, 1, 0]
    [1, 1, 1, 0, 1, 1, 0, 1, 0, 0]

  Compare with regular_sat.py which use my decomposition of MiniZinc style 
  of the global constraint regular(_table). The transitions are quite
  different than for AddAutomaton.


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import ListPrinter


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, reg_input):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__reg_input = reg_input
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        # Translate 1/2 to 0/1
        print('reg_input:', [self.Value(v) for v in self.__reg_input])

    def SolutionCount(self):
        return self.__solution_count


#
# Make a transition (automaton) matrix from a
# single pattern, e.g. [3,2,1]
#
def make_transition_matrix(pattern=[3,2,1]):

  t = []
  c = 0
  for i in range(len(pattern)):
    t.append((c,0,c))  # 0*
    for _j in range(pattern[i]):      
      t.append((c,1,c+1)) # 1{pattern[i]}
      c += 1

    t.append((c,0,c+1)) # 0+
    
    c+=1
  
  t.append((c,0,c)) # 0*

  return t, c


def main(pp=[3,2,1],this_len=10):

  model = cp.CpModel()

  #
  # data
  #

  # this_len = 10
  # pp = [3, 2, 1]
  print("pp:",pp, "this_len:", this_len)
  
  transitions,last_state = make_transition_matrix(pp)
  print("transitions:",transitions)

  # This works!

  # Hard coded
  #        0*1+0*
  # state  0 1 2 
  #
  #   from output to
  #     0   0      0
  #     0   1      1
  #     1   1      1
  #     1   0      2
  #     2   0      2
  #  start state: 0
  #  accepting state = 2         
  # OK!
  # transitions = [
  #     (0,0,0),
  #     (0,1,1),
  #     (1,1,1),
  #     (1,0,2),
  #     (2,0,2)
  # ]
  # print("transitions:", transitions)
  initial_state = 0
  print("last_state:", last_state)
  accepting_states = [last_state,last_state-1]
  # accepting_states = [last_state]  
  print("accepting_states:",accepting_states)

  # variables
  x = [model.NewIntVar(0, 1, 'x[%i]' % i) for i in range(this_len)]

  #
  # constraints
  #
  # AddAutomaton(self, transition_variables, starting_state, final_states, transition_triples)
  model.AddAutomaton(x, initial_state, accepting_states, transitions)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)
  print("status:", solver.StatusName(status))
  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())

# The pattern: 
# 1{3}0+1{2}0+1{1}0*
pp = [3,2,1] 
# pp = [2,1] 
n = 11
if __name__ == '__main__':
  main(pp,n)
