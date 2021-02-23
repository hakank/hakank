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

  Nonogram (Painting by numbers) in OR-tools CP-SAT Solver.

  http://en.wikipedia.org/wiki/Nonogram
  '''
  Nonograms or Paint by Numbers are picture logic puzzles in which cells in a
  grid have to be colored or left blank according to numbers given at the
  side of the grid to reveal a hidden picture. In this puzzle type, the
  numbers measure how many unbroken lines of filled-in squares there are
  in any given row or column. For example, a clue of '4 8 3' would mean
  there are sets of four, eight, and three filled squares, in that order,
  with at least one blank square between successive groups.

  '''

  See problem 12 at http://www.csplib.org/.

  http://www.puzzlemuseum.com/nonogram.htm

  Haskell solution:
  http://twan.home.fmf.nl/blog/haskell/Nonograms.details

  Brunetti, Sara & Daurat, Alain (2003)
  'An algorithm reconstructing convex lattice sets'
  http://geodisi.u-strasbg.fr/~daurat/papiers/tomoqconv.pdf


  The Comet model (http://www.hakank.org/comet/nonogram_regular.co)
  was a major influence when writing this model.

  I have also blogged about the development of a Nonogram solver in Comet
  using the regular constraint.
  * 'Comet: Nonogram improved: solving problem P200 from 1:30 minutes
     to about 1 second'
     http://www.hakank.org/constraint_programming_blog/2009/03/comet_nonogram_improved_solvin_1.html

  * 'Comet: regular constraint, a much faster Nonogram with the regular
  constraint,
     some OPL models, and more'
     http://www.hakank.org/constraint_programming_blog/2009/02/comet_regular_constraint_a_muc_1.html


  This is a port of my old CP model nonogram_table.py

  The old CP model nonogram_regular.py was almost identical to nonogram_table.py,
  with the difference that it used the `regular_element` instead of `regular_table`.
  This model incorporates selection of these constraints via the second 
  argument of the program, e.g.:

     $ python3 nonogram_table_sat.py nonogram_pbn_merka.py regular_element
     $ python3 nonogram_table_sat.py nonogram_pbn_merka.py regular_table

  The default is to use regular_table which seems to be quite faster than
  regular_element.

  Note: All methods are defined in nonogram_utils_sat.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import regular_table, regular_element
from nonogram_utils_sat import SolutionPrinter, make_transition_matrix, check_rule, run_nonogram


def main(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules, regular_method):
  run_nonogram(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules, regular_method)

#
# Default problem
#
# From http://twan.home.fmf.nl/blog/haskell/Nonograms.details
# The lambda picture
#
rows = 12
row_rule_len = 3
row_rules = [[0, 0, 2], [0, 1, 2], [0, 1, 1], [0, 0, 2], [0, 0, 1], [0, 0, 3],
             [0, 0, 3], [0, 2, 2], [0, 2, 1], [2, 2, 1], [0, 2, 3], [0, 2, 2]]

cols = 10
col_rule_len = 2
col_rules = [[2, 1], [1, 3], [2, 4], [3, 4], [0, 4], [0, 3], [0, 3], [0, 3],
             [0, 2], [0, 2]]

regular_method = "regular_table"
if __name__ == '__main__':
  if len(sys.argv) > 1:
    file = sys.argv[1]
    # If one accidentally forgot to add a file
    # as first argument.
    if file in ["regular","regular_table"]:
      regular_method = file
    else:
      exec(compile(open(file).read(), file, 'exec'))
  if len(sys.argv) > 2:
    m = sys.argv[2]
    if m in ["regular","regular_table"]:
      regular_method = m
  print(f"Using regular method `{regular_method}`.")
  main(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules, regular_method)
