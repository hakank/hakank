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

  Labeled dice problem in OR-tools CP-SAT Solver.

  From Jim Orlin 'Colored letters, labeled dice: a logic puzzle'
  http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
  '''
  My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There
  are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY,
  JUNK, LIMN, QUIP, SWAG, VISA, WISH.

  There are 24 different letters that appear in the 13 words.  The question
  is:  can one assign the 24 letters to 4 different cubes so that the
  four letters of each word appears on different cubes.  (There is one
  letter from each word on each cube.)  It might be fun for you to try
  it.  I'll give a small hint at the end of this post. The puzzle was
  created by Humphrey Dudley.
  '''

  Jim Orlin's followup 'Update on Logic Puzzle':
  http://jimorlin.wordpress.com/2009/02/21/update-on-logic-puzzle/


  This is a port of my old CP model labeled_dice.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import count_vars

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, dice, letters, words, num_words, n, m):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__dice = dice 
        self.__letters = letters
        self.__words = words
        self.__num_words = num_words 
        self.__n = n
        self.__m = m
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print(f"Solution #{self.__solution_count}")
        for d in range(self.__n):
          print("die %i:" % d, end=" ")
          for i in range(self.__m):
            if self.Value(self.__dice[i]) == d:
              print(self.__letters[i], end=" ")
          print()

        print("The words with the cube label:")
        for i in range(self.__num_words):
          for j in range(self.__n):
            print(
                "%s (%i)" % (self.__letters[self.__words[i][j]], 
                self.Value(self.__dice[self.__words[i][j]])),
                end=" ")
          print()

        print()

    def SolutionCount(self):
        return self.__solution_count



def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 4
  m = 24
  A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, Y = (
      list(range(m)))
  letters = [
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
      "P", "Q", "R", "S", "T", "U", "V", "W", "Y"
  ]

  num_words = 13
  words = [[B, U, O, Y], [C, A, V, E], [C, E, L, T], [F, L, U, B], [F, O, R, K],
           [H, E, M, P], [J, U, D, Y], [J, U, N, K], [L, I, M, N], [Q, U, I, P],
           [S, W, A, G], [V, I, S, A], [W, I, S, H]]

  #
  # declare variables
  #
  dice = [model.NewIntVar(0, n - 1, "dice[%i]" % i) for i in range(m)]

  #
  # constraints
  #

  # the letters in a word must be on a different die
  for i in range(num_words):
    model.AddAllDifferent([dice[words[i][j]] for j in range(n)])

  # there must be exactly 6 letters of each die
  for i in range(n):
    count_vars(model,[dice[j] for j in range(m)], i, 6 )

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  solution_printer = SolutionPrinter(dice, letters, words, num_words, n, m)
  status = solver.SearchForAllSolutions(model, solution_printer)

  #
  # result
  #
  if status != cp.OPTIMAL:
    print("No solution")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
