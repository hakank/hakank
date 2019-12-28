#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# A Round of Golf puzzle (Dell Logic Puzzles) in Z3
#
# From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
# '''
# Title: A Round of Golf
# Author: Ellen K. Rodehorst
# Publication: Dell Favorite Logic Problems
# Issue: Summer, 2000
# Puzzle #: 9
# Stars: 1
#
# When the Sunny Hills Country Club golf course isn't in use by club members,
# of course, it's open to the club's employees. Recently, Jack and three other
# workers at the golf course got together on their day off to play a round of
# eighteen holes of golf.
# Afterward, all four, including Mr. Green, went to the clubhouse to total
# their scorecards. Each man works at a different job (one is a short-order
# cook), and each shot a different score in the game. No one scored below
# 70 or above 85 strokes. From the clues below, can you discover each man's
# full name, job and golf score?
#
# 1. Bill, who is not the maintenance man, plays golf often and had the lowest
# score of the foursome.
# 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten
# strokes more than the pro-shop clerk.
# 3. In some order, Frank and the caddy scored four and seven more strokes than
# Mr. Sands.
# 4. Mr. Carter thought his score of 78 was one of his better games, even
#    though Frank's score  was lower.
# 5. None of the four scored exactly 81 strokes.
#
# Determine: First Name - Last Name - Job - Score
# '''
#
# Compare with the F1 model:
# http://www.f1compiler.com/samples/A 20Round 20of 20Golf.f1.html

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

sol = Solver()

n = 4

Jack, Bill, Paul, Frank = [0,1,2,3]
first_name = [Jack, Bill, Paul, Frank]

# Decision variables
last_name = makeIntArrayVector(sol,"last_name",n,0,n-1)
[Green,Clubb,Sands,Carter] = last_name

job = makeIntArrayVector(sol,"job",n,0,n-1)
[cook,maintenance_man,clerk,caddy] = job

score = makeIntArray(sol,"score",n,70,85)

# Constraints
sol.add(Distinct([last_name[i] for i in range(n)]))
sol.add(Distinct([job[i] for i in range(n)]))
sol.add(Distinct([score[i] for i in range(n)]))

# 1. Bill, who is not the maintenance man, plays golf often and had 
#    the lowest score of the foursome.
sol.add(Bill!= maintenance_man)

sol.add(score[Bill] < score[Jack])
sol.add(score[Bill] < score[Paul])
sol.add(score[Bill] < score[Frank])

# 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
#    scored ten strokes more than the pro-shop clerk.
sol.add(Clubb != Paul)
sol.add(score[Clubb] == score[clerk] + 10)


# 3. In some order, Frank and the caddy scored four and seven more 
#    strokes than Mr. Sands.
sol.add(Frank != caddy)
sol.add(Frank != Sands)
sol.add(caddy != Sands)
sol.add(
    Or(
       And(score[Frank] == score[Sands] + 4, score[caddy] == score[Sands] + 7 )
       ,
       And(score[Frank] == score[Sands] + 7, score[caddy] == score[Sands] + 4 )
    ))

# 4. Mr. Carter thought his score of 78 was one of his better games, even 
#    though Frank's score was lower.
sol.add(Frank != Carter)
sol.add(score[Carter] == 78)
sol.add(score[Frank] < score[Carter])

# 5. None of the four scored exactly 81 strokes.
for i in range(n):
  sol.add(score[i] != 81)


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("last_name:", [mod.eval(last_name[i]) for i in range(n)])
  print("job      :", [mod.eval(job[i]) for i in range(n)])
  print("score    :", [mod.eval(score[i]) for i in range(n)])
  sol.add(
      Or(
         Or([last_name[i] != mod.eval(last_name[i]) for i in range(n)]),
         Or([job[i] != mod.eval(job[i]) for i in range(n)]),
         Or([score[i] != mod.eval(score[i]) for i in range(n)])
         )
      )
  print()

print("num_solutions:", num_solutions)

        


