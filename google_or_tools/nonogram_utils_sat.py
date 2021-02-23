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

  Utilities for Nonogram solver in OR-tools CP-SAT Solver.

  See nonogram_table_sat.py for examples etc how to use this 
  package.

  This package was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import regular_table, regular_element



class SolutionPrinter(cp.CpSolverSolutionCallback):
    """
    SolutionPrinter for Nonogram.
    """
    def __init__(self, board, rows, cols,):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__board = board
        self.__rows = rows
        self.__cols = cols
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print()
        for i in range(self.__rows):
          row = [self.Value(self.__board[i, j]) - 1 for j in range(self.__cols)]
          row_pres = []
          for j in row:
            if j == 1:
              row_pres.append('#')
            else:
              row_pres.append(' ')
          print('  ', ''.join(row_pres))

        print()
        # print('  ', '-' * self.__cols)
        print()

        if self.__solution_count >= 2:
          print('2 solutions is enough...')
          self.StopSearch()

    def SolutionCount(self):
        return self.__solution_count


def make_transition_matrix(pattern):
  """
  Make a transition (automaton) matrix from a
  single pattern, e.g. [3,2,1]
  """

  p_len = len(pattern)
  num_states = p_len + sum(pattern)

  # this is for handling 0-clues. It generates
  # just the state 1,2
  if num_states == 0:
    num_states = 1

  t_matrix = []
  for i in range(num_states):
    row = []
    for _j in range(2):
      row.append(0)
    t_matrix.append(row)

  # convert pattern to a 0/1 pattern for easy handling of
  # the states
  tmp = [0 for i in range(num_states)]
  c = 0
  tmp[c] = 0
  for i in range(p_len):
    for _j in range(pattern[i]):
      c += 1
      tmp[c] = 1
    if c < num_states - 1:
      c += 1
      tmp[c] = 0

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

  # print 'The states:'
  # for i in range(num_states):
  #     for j in range(2):
  #         print t_matrix[i][j],
  #     print
  # print

  return t_matrix


def check_rule(model, rules, y, regular_method):
  """
  Check each rule by creating an automaton
  and then run the regular constraint.
  """

  # r_len = sum([1 for i in range(len(rules)) if rules[i] > 0])
  rules_tmp = []
  for i in range(len(rules)):
    if rules[i] > 0:
      rules_tmp.append(rules[i])

  transition_fn = make_transition_matrix(rules_tmp)
  n_states = len(transition_fn)
  input_max = 2

  # Note: we cannot use 0 since it's the failing state
  initial_state = 1
  accepting_states = [n_states]  # This is the last state

  if regular_method == "regular_element":
    regular_element(model, y, n_states, input_max, transition_fn, initial_state,
                accepting_states)
  else:
    regular_table(model, y, n_states, input_max, transition_fn, initial_state,
                accepting_states)
  



def run_nonogram(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules, regular_method):
  """
  Run a Nonogram instance.
  """
  model = cp.CpModel()

  #
  # data
  #

  #
  # variables
  #
  board = {}
  for i in range(rows):
    for j in range(cols):
      board[i, j] = model.NewIntVar(1, 2, 'board[%i, %i]' % (i, j))

  # Flattened board for labeling.
  # This labeling was inspired by a suggestion from
  # Pascal Van Hentenryck about my Comet nonogram model.
  board_label = []
  if rows * row_rule_len < cols * col_rule_len:
    for i in range(rows):
      for j in range(cols):
        board_label.append(board[i, j])
  else:
    for j in range(cols):
      for i in range(rows):
        board_label.append(board[i, j])

  #
  # constraints
  #
  for i in range(rows):
    check_rule(model, [row_rules[i][j] for j in range(row_rule_len)],
               [board[i, j] for j in range(cols)],regular_method)

  for j in range(cols):
    check_rule(model, [col_rules[j][k] for k in range(col_rule_len)],
               [board[i, j] for i in range(rows)],regular_method)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  # solver.parameters.linearization_level = 0
  # solver.parameters.cp_model_probing_level = 0

  solution_printer = SolutionPrinter(board, rows, cols)
  status = solver.SearchForAllSolutions(model, solution_printer)

  print("status:", solver.StatusName(status))
  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution")

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime(), 'ms')
