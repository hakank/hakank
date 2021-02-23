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
  Hidato puzzle in OR-tools CP-SAT Solver.

  http://www.shockwave.com/gamelanding/hidato.jsp
  http://www.hidato.com/
  '''
  Puzzles start semi-filled with numbered tiles.
  The first and last numbers are circled.
  Connect the numbers together to win. Consecutive
  number must touch horizontally, vertically, or
  diagonally.
  '''

  This is a port of my old CP model hidato.py

  Note: This model is _much_ faster than the CP 
  model: It solve all problem instances in ~ 0.8s
  

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main(r, c):

  model = cp.CpModel()

  # data
  # Simple problem
  if r == 3 and c == 3:
    puzzle = [[6, 0, 9], 
              [0, 2, 8], 
              [1, 0, 0]]

  if r == 7 and c == 7:
    puzzle = [[0, 44, 41, 0, 0, 0, 0], 
              [0, 43, 0, 28, 29, 0, 0],
              [0, 1, 0, 0, 0, 33, 0], 
              [0, 2, 25, 4, 34, 0, 36],
              [49, 16, 0, 23, 0, 0, 0], 
              [0, 19, 0, 0, 12, 7, 0],
              [0, 0, 0, 14, 0, 0, 0]]

  # Problems from the book:
  # Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

  # Problem 1 (Practice)
  # r = 5
  # c = r
  # puzzle = [
  #    [ 0, 0,20, 0, 0],
  #    [ 0, 0, 0,16,18],
  #    [22, 0,15, 0, 0],
  #    [23, 0, 1,14,11],
  #    [ 0,25, 0, 0,12],
  #    ]

  # Problem 2 (Practice)
  if r == 5 and c == 5:
    puzzle = [
        [0, 0, 0, 0, 14],
        [0, 18, 12, 0, 0],
        [0, 0, 17, 4, 5],
        [0, 0, 7, 0, 0],
        [9, 8, 25, 1, 0],
    ]

  # Problem 3 (Beginner)
  if r == 6 and c == 6:
    puzzle = [[0, 26, 0, 0, 0, 18], 
              [0, 0, 27, 0, 0, 19], 
              [31, 23, 0, 0, 14, 0],
              [0, 33, 8, 0, 15, 1], 
              [0, 0, 0, 5, 0, 0], 
              [35, 36, 0, 10, 0, 0]]

  # Problem 15 (Intermediate)
  if r == 8 and c == 8:
    puzzle = [[64, 0, 0, 0, 0, 0, 0, 0], 
              [1, 63, 0, 59, 15, 57, 53, 0],
              [0, 4, 0, 14, 0, 0, 0, 0], 
              [3, 0, 11, 0, 20, 19, 0, 50],
              [0, 0, 0, 0, 22, 0, 48, 40], 
              [9, 0, 0, 32, 23, 0, 0, 41],
              [27, 0, 0, 0, 36, 0, 46, 0], 
              [28, 30, 0, 35, 0, 0, 0, 0]]

  # Problem 156 (Master}
  # (This is harder to solve than the 12x12 prolem 188 below...%}
  if r == 10 and c == 10:
    puzzle = [[88, 0, 0,100, 0, 0,37,0, 0,34],
              [ 0,86, 0,96,41, 0, 0,36, 0, 0],
              [ 0,93,95,83, 0, 0, 0,31,47, 0],
              [ 0,91, 0, 0, 0, 0, 0,29, 0, 0],
              [11, 0, 0, 0, 0, 0, 0,45,51, 0],
              [ 0, 9, 5, 3, 1, 0, 0, 0, 0, 0],
              [ 0,13, 4, 0, 0, 0, 0, 0, 0, 0],
              [15, 0, 0,25, 0, 0,54,67, 0, 0],
              [ 0,17, 0,23, 0,60,59, 0,69, 0],
              [19, 0,21,62,63, 0, 0, 0, 0, 0]]


  #  Problem 188 (Genius]
  if r == 12 and c == 12:
    puzzle = [[  0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0],
              [136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0],
              [139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0],
              [  0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103],
              [  0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101],
              [  0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0],
              [ 30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0],
              [ 32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92],
              [  0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0],
              [  0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0],
              [ 35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0],
              [ 36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88]]


  print_game(puzzle, r, c)

  #
  # declare variables
  #
  x = {}
  for i in range(r):
    for j in range(c):
      x[(i, j)] = model.NewIntVar(1, r * c, "dice(%i,%i)" % (i, j))
  x_flat = [x[(i, j)] for i in range(r) for j in range(c)]

  #
  # constraints
  #
  model.AddAllDifferent(x_flat)

  #
  # Fill in the clues
  #
  for i in range(r):
    for j in range(c):
      if puzzle[i][j] > 0:
        model.Add(x[(i, j)] == puzzle[i][j])

  # From the numbers k = 1 to r*c-1, find this position,
  # and then the position of k+1
  for k in range(1, r * c):
    i = model.NewIntVar(0, r,"i")
    j = model.NewIntVar(0, c,"j")
    a = model.NewIntVar(-1, 1,"a")
    b = model.NewIntVar(-1, 1, "b")

    # 1) First: fix "this" k
    # k == x[(i,j)]
    ic_plus_j = model.NewIntVar(0,r*c*2,"ic_plus_j")
    model.Add(ic_plus_j == i*c + j)
    model.AddElement(ic_plus_j, x_flat, k)
    # 2) and then find the position of the next value (k+1)
    # k + 1 == x[(i+a,j+b)]
    iacjb = model.NewIntVar(0,r*c*2,"iacjb")
    model.Add(iacjb == (i + a) * c + (j + b))
    model.AddElement(iacjb, x_flat, k + 1)

    # i in 0..r-1, c in 0..c-i
    model.Add(i + a >= 0)
    model.Add(j + b >= 0)
    model.Add(i + a < r)
    model.Add(j + b < c)

    # and (((a != 0) | (b != 0)))
    a_nz = model.NewBoolVar("a_nz")
    b_nz = model.NewBoolVar("b_nz")
    model.Add(a != 0).OnlyEnforceIf(a_nz)
    model.Add(a == 0).OnlyEnforceIf(a_nz.Not())
    model.Add(b != 0).OnlyEnforceIf(b_nz)
    model.Add(b == 0).OnlyEnforceIf(b_nz.Not())
    model.Add(a_nz + b_nz >= 1)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.num_search_workers = 8 # not needed

  status = solver.Solve(model)
  if status == cp.OPTIMAL:
    print_board(solver, x, r, c)

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


def print_board(solver, x, rows, cols):
  print("Solution:")
  for i in range(rows):
    for j in range(cols):
      print("% 3s" % solver.Value(x[i, j]), end=" ")
    print()
  print()


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 3s" % game[i][j], end=" ")
    print("")
  print()

def solve_all():
  all_problems = [
    [3,3],
    [5,5],
    [6,6],
    [7,7],
    [8,8],
    [10,10],
    [12,12]
  ]
  for r,c in all_problems:
    print(f"\n\nProblem {r} x {c}:")
    main(r,c)

if __name__ == "__main__":
  solve_all()
