"""
A Round of Golf puzzle (Dell Logic Puzzles) in cpmpy.

From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
'''
Title: A Round of Golf
Author: Ellen K. Rodehorst
Publication: Dell Favorite Logic Problems
Issue: Summer, 2000
Puzzle #: 9
Stars: 1

When the Sunny Hills Country Club golf course isn't in use by club members,
of course, it's open to the club's employees. Recently, Jack and three other
workers at the golf course got together on their day off to play a round of
eighteen holes of golf.
Afterward, all four, including Mr. Green, went to the clubhouse to total
their scorecards. Each man works at a different job (one is a short-order
cook), and each shot a different score in the game. No one scored below
70 or above 85 strokes. From the clues below, can you discover each man's
full name, job and golf score?

1. Bill, who is not the maintenance man, plays golf often and had the lowest
score of the foursome.
2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten
strokes more than the pro-shop clerk.
3. In some order, Frank and the caddy scored four and seven more strokes than
Mr. Sands.
4. Mr. Carter thought his score of 78 was one of his better games, even
  though Frank's score  was lower.
5. None of the four scored exactly 81 strokes.

Determine: First Name - Last Name - Job - Score
'''

Compare with the F1 model:
http://www.f1compiler.com/samples/A 20Round 20of 20Golf.f1.html


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def a_round_of_golf():

  model = Model()

  #
  # data
  #
  n = 4
  [Jack, Bill, Paul, Frank] = [i for i in range(n)]

  #
  # declare variables
  #
  last_name = intvar(0,n-1,shape=n,name="last_name")
  [Green, Clubb, Sands, Carter] = last_name

  job = intvar(0,n-1,shape=n,name="job")
  [cook, maintenance_man, clerk, caddy] = job

  score = intvar(70,85,shape=n,name="score")

  #
  # constraints
  #
  model += [AllDifferent(last_name)]
  model += [AllDifferent(job)]
  model += [AllDifferent(score)]

  # 1. Bill, who is not the maintenance man, plays golf often and had
  #    the lowest score of the foursome.
  model += [Bill != maintenance_man]
  model += [score[Bill] < score[Jack]]
  model += [score[Bill] < score[Paul]]
  model += [score[Bill] < score[Frank]]

  # 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and
  #    scored ten strokes more than the pro-shop clerk.
  model += [Clubb != Paul]
  model += [score[Clubb] == score[clerk] + 10]

  # 3. In some order, Frank and the caddy scored four and seven more
  #    strokes than Mr. Sands.
  model += [Frank != caddy]
  model += [Frank != Sands]
  model += [caddy != Sands]
  model += [( (score[Frank] == score[Sands] + 4) & (score[caddy] == score[Sands] + 7)) |
            ( (score[Frank] == score[Sands] + 7) & (score[caddy] == score[Sands] + 4)) ]


  # 4. Mr. Carter thought his score of 78 was one of his better games,
  #    even though Frank's score was lower.
  model += [Frank != Carter]
  model += [score[Carter] == 78]
  model += [score[Frank] < score[Carter]]

  # 5. None of the four scored exactly 81 strokes.
  model += [score != 81]

  def print_sol():
    print("last_name:", last_name.value())
    print("job      :", job.value())
    print("score    :", score.value())
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


a_round_of_golf()
