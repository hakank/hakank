#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Lecture Series Puzzle (Dell Logic Puzzles) in Z3
#
# Problem from
# http://brownbuffalo.sourceforge.net/LectureSeriesClues.html
# """
# Title: Lecture Series
# Author: Alex Knight
# Publication: Dell Logic Puzzles
# Issue: April, 1998
# Page: 10
# Stars: 2
#
# Last week at school was made varied by a series of lectures, one each 
# day (Monday through Friday), in the auditorium. None of the lectures was 
# particularly interesting (on choosing a college, physical hygiene, modern 
# art, nutrition, and study habits), but the students figured that anything 
# that got them out of fourth period was okay. The lecturers were two women 
# named Alice and Bernadette, and three men named Charles, Duane, and Eddie; 
# last names were Felicidad, Garber, Haller, Itakura, and Jeffreys. 
# Can you find each day's lecturer and subject?
#
# 1. Alice lectured on Monday.
# 2. Charles's lecture on physical hygiene wasn't given on Friday.
# 3. Dietician Jeffreys gave the lecture on nutrition.
# 4. A man gave the lecture on modern art.
# 5. Ms. Itakura and the lecturer on proper study habits spoke on 
#    consecutive days, in one order or the other.
# 6. Haller gave a lecture sometime after Eddie did.
# 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("QF_FD")

n = 5

days = range(n)
[Monday, Tuesday, Wednesday, Thursday, Friday] = days

# variables
lectures = makeIntArray(sol,"lectures", n, 0,n-1)
[choosing_a_college, physical_hygiene, modern_art, nutrition, study_habits] = [lectures[i] for i in range(n)]


first_name = makeIntArray(sol,"first_name", n, 0,n-1)
[Alice, Bernadette, Charles, Duane, Eddie] = [first_name[i] for i in range(n)]

last_name = makeIntArray(sol,"last_name", n, 0,n-1)
[Felicidad, Garber, Haller, Itakura, Jeffreys] = [last_name[i] for i in range(n)]

# constraints

sol.add(Distinct([lectures[i] for i in range(n)]))
sol.add(Distinct([first_name[i] for i in range(n)]))
sol.add(Distinct([last_name[i] for i in range(n)]))

# 1. Alice lectured on Monday.
sol.add(Alice == Monday)

# 2. Charles's lecture on physical hygiene wasn't given on Friday.
sol.add(Charles == physical_hygiene)
sol.add(Charles != Friday)
sol.add(physical_hygiene != Friday )

# 3. Dietician Jeffreys gave the lecture on nutrition.
sol.add(Jeffreys == nutrition)

# 4. A man gave the lecture on modern art.
sol.add(Or( 
  modern_art == Charles,
  modern_art == Duane, 
  modern_art == Eddie
))

# 5. Ms. Itakura and the lecturer on proper study habits spoke on 
#    consecutive days, in one order or the other.
sol.add(Or(Itakura == Alice,Itakura == Bernadette))

sol.add(Abs(Itakura - study_habits) == 1)
sol.add(Itakura != study_habits)

# 6. Haller gave a lecture sometime after Eddie did.
sol.add(Haller > Eddie)

# 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
sol.add(Duane == Felicidad)
sol.add(Duane < modern_art)


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("days      :", days)
  print("lectures  :", [mod.eval(lectures[i]) for i in range(n)])
  print("first_name:", [mod.eval(first_name[i]) for i in range(n)])
  print("last_name :", [mod.eval(last_name[i]) for i in range(n)])
  print()
  getDifferentSolution(sol,mod,
                       [lectures[i] for i in range(n)],
                       [first_name[i] for i in range(n)],
                       [last_name[i] for i in range(n)]
                       )

print("num_solutions:", num_solutions)
