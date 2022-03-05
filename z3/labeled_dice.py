#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Labeled dice and Building block problems in Z3
#
# * Labeled dice
#
#   From Jim Orlin 'Colored letters, labeled dice: a logic puzzle'
#   http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
#   '''
#   My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There
#   are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY,
#   JUNK, LIMN, QUIP, SWAG, VISA, WISH.
#
#   There are 24 different letters that appear in the 13 words.  The question
#   is:  can one assign the 24 letters to 4 different cubes so that the
#   four letters of each word appears on different cubes.  (There is one
#   letter from each word on each cube.)  It might be fun for you to try
#   it.  I'll give a small hint at the end of this post. The puzzle was
#   created by Humphrey Dudley.
#   '''
#
#   Also, see Jim Orlin's followup 'Update on Logic Puzzle':
#   http://jimorlin.wordpress.com/2009/02/21/update-on-logic-puzzle/
#
#
# * Building Blocks puzzle (Dell Logic Puzzles) in MiniZinc.
# 
#   From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
#   """
#   Each of four alphabet blocks has a single letter of the alphabet on each 
#   of its six sides. In all, the four blocks contain every letter but 
#   Q and Z. By arranging the blocks in various ways, you can spell all of 
#   the words listed below. Can you figure out how the letters are arranged 
#   on the four blocks?
#
#   BAKE ONYX ECHO OVAL
#
#   GIRD SMUG JUMP TORN
#
#   LUCK VINY LUSH WRAP
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *


def labeled_dice():

  print("Labeled dice\n")

  #
  # data
  #
  n = 4
  m = 24
  A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, Y = (
      list(range(m)))
  letters = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, Y]
  letters_s = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
             "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "Y"]

  num_words = 13
  words = [
      [B,U,O,Y],
      [C,A,V,E],
      [C,E,L,T],
      [F,L,U,B],
      [F,O,R,K],
      [H,E,M,P],
      [J,U,D,Y],
      [J,U,N,K],
      [L,I,M,N],
      [Q,U,I,P],
      [S,W,A,G],
      [V,I,S,A],
      [W,I,S,H]
      ]
  
  solve_it(n,m,letters,letters_s,num_words,words)



def building_blocks():
  print("Building blocks\n")
  n = 4
  m = 24
  A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R, S, T, U, V, W, X, Y = (
      list(range(m)))
  letters = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R, S, T, U, V, W, X, Y]
  letters_s = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
             "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X","Y"]

  num_words = 12
  words = [
      [B,A,K,E],
      [O,N,Y,X],
      [E,C,H,O],
      [O,V,A,L],
      [G,I,R,D],
      [S,M,U,G],
      [J,U,M,P],
      [T,O,R,N],
      [L,U,C,K],
      [V,I,N,Y],
      [L,U,S,H],
      [W,R,A,P]
  ]
  
  solve_it(n,m,letters,letters_s,num_words,words)
   


def solve_it(n,m,letters,letters_s,num_words,words):

  sol = SolverFor("QF_FD")
    
  #
  # declare variables
  #
  dice = [makeIntVar(sol, "dice[%i]" % i, 0, n - 1) for i in range(m)]

  # constraints

  # the letters in a word must be on a different die
  for i in range(num_words):
    sol.add(Distinct([dice[words[i][j]] for j in range(n)]))

  # there must be exactly 6 letters of each die
  for i in range(n):
    sol.add(Sum([If(dice[j] == i,1,0) for j in range(m)]) == 6)

  #
  # solution and search

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for d in range(n):
      print("die %i:" % d, end=' ')
      for i in range(m):
        if mod.eval(dice[i]) == d:
          print(letters[i], end=' ')
      print()
    print("The words with the cube label:")
    for i in range(num_words):
      for j in range(n):
        print("%s (%i)" % (letters_s[words[i][j]], mod.eval(dice[words[i][j]]).as_long()), end=' ')
      print()
    sol.add(Or([dice[i] != mod.eval(dice[i]) for i in range(m)]))
    print()

  print()
  print("num_solutions:", num_solutions)



if __name__ == "__main__":
  labeled_dice()
  print("\n\n\n")
  building_blocks()
