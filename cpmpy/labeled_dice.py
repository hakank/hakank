"""
Labeled dice problem in cpmpy.

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


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def labeled_dice():

  model = Model()
  
  # data
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

  # declare variables
  dice = intvar(0,n-1,shape=m,name="dice")

  # constraints

  # the letters in a word must be on a different die
  for i in range(num_words):
    # model += [AllDifferent([dice[words[i][j]] for j in range(n)])]
    model += [AllDifferent(dice[words[i]])]

  # there must be exactly 6 letters of each die
  for i in range(n):
    model += [sum([dice[j] == i for j in range(m)]) == 6]

  def print_sol():
    for d in range(n):
      print("die %i:" % d, end=" ")
      for i in range(m):
        if dice[i].value() == d:
          print(letters[i], end=" ")
      print()

    print("The words with the cube label:")
    for i in range(num_words):
      for j in range(n):
        print(
            "%s (%i)" % (letters[words[i][j]], dice[words[i][j]].value()),
            end=" ")
      print()
    print()
  

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


labeled_dice()
