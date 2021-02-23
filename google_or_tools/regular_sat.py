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

  Global constraint regular in OR-tools CP-SAT Solver.

  This is a translation of MiniZinc's regular constraint (defined in
  lib/zinc/globals.mzn). All comments are from the MiniZinc code.
  '''
  The sequence of values in array 'x' (which must all be in the range 1..S)
  is accepted by the DFA of 'Q' states with input 1..S and transition
  function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
  (which must be in 1..Q) and accepting states 'F' (which all must be in
  1..Q).  We reserve state 0 to be an always failing state.
  '''

  Here we test with the following regular expression:
    0*1{3}0+1{2}0+1{1}0*
  using an array of size 10.

  We also show the two variants of the regular constraint:
    - regular_table, which is faster than
    - regular_element

  For a more extensive use of the regular_table (/regular_element)
  constraint, see the Nonogram solver nonogram_table_sat.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import regular_table, regular_element


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, reg_input):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__reg_input = reg_input
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        # Translate 1/2 to 0/1
        print('reg_input:', [self.Value(v) - 1 for v in self.__reg_input])

    def SolutionCount(self):
        return self.__solution_count


#
# Make a transition (automaton) matrix from a
# single pattern, e.g. [3,2,1]
#
def make_transition_matrix(pattern):

  p_len = len(pattern)
  print('p_len:', p_len)
  num_states = p_len + sum(pattern)
  print('num_states:', num_states)
  t_matrix = []
  for i in range(num_states):
    row = []
    for j in range(2):
      row.append(0)
    t_matrix.append(row)

  # convert pattern to a 0/1 pattern for easy handling of
  # the states
  tmp = [0 for i in range(num_states)]
  c = 0
  tmp[c] = 0
  for i in range(p_len):
    for j in range(pattern[i]):
      c += 1
      tmp[c] = 1
    if c < num_states - 1:
      c += 1
      tmp[c] = 0
  print('tmp:', tmp)

  t_matrix[num_states - 1][0] = num_states
  t_matrix[num_states - 1][1] = 0

  for i in range(num_states):
    if tmp[i] == 0:
      t_matrix[i][0] = i + 1
      t_matrix[i][1] = i + 2
    else:
      if i < num_states - 1:
        if tmp[i + 1] == 1:
          t_matrix[i][0] = 0
          t_matrix[i][1] = i + 2
        else:
          t_matrix[i][0] = i + 2
          t_matrix[i][1] = 0

  print('The states:')
  for i in range(num_states):
    for j in range(2):
      print(t_matrix[i][j], end=' ')
    print()
  print()

  return t_matrix


def main(pp=[3,2,1],this_len=10,use_regular_table=True):

  model = cp.CpModel()

  #
  # data
  #

  # this_len = 10
  # pp = [3, 2, 1]
  print("pp:",pp, "this_len:", this_len)
  
  transition_fn = make_transition_matrix(pp)
  n_states = len(transition_fn)
  input_max = 2

  # Note: we use '1' and '2' (rather than 0 and 1)
  # since 0 represents the failing state.
  initial_state = 1

  accepting_states = [n_states]

  # declare variables
  reg_input = [
      model.NewIntVar(1, input_max, 'reg_input[%i]' % i) for i in range(this_len)
  ]

  #
  # constraints
  #
  if use_regular_table:
    regular_table(model, reg_input, n_states, input_max, transition_fn, initial_state,
          accepting_states)
  else:
    regular_element(model, reg_input, n_states, input_max, transition_fn, initial_state,
          accepting_states)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(reg_input)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if status != cp.OPTIMAL:
    print("No solution!")

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())

# The pattern: 
# 1{3}0+1{2}0+1{1}0*
pp = [3,2,1] 
n = 10
if __name__ == '__main__':
  print("Use regular_table constraint:")
  main(pp,n,True)
  print()
  print("Use regular_element constraint:")
  main(pp,n, False)
