"""
Lecture Series Puzzle (Dell Logic Puzzles) in cpmpy.
 
Problem from http://brownbuffalo.sourceforge.net/LectureSeriesClues.html
'''
Title: Lecture Series
Author: Alex Knight
Publication: Dell Logic Puzzles
Issue: April, 1998
Page: 10
Stars: 2

Last week at school was made varied by a series of lectures, one each 
day (Monday through Friday), in the auditorium. None of the lectures was 
particularly interesting (on choosing a college, physical hygiene, modern 
art, nutrition, and study habits), but the students figured that anything 
that got them out of fourth period was okay. The lecturers were two women 
named Alice and Bernadette, and three men named Charles, Duane, and Eddie; 
last names were Felicidad, Garber, Haller, Itakura, and Jeffreys. 
Can you find each day's lecturer and subject?

1. Alice lectured on Monday.
2. Charles's lecture on physical hygiene wasn't given on Friday.
3. Dietician Jeffreys gave the lecture on nutrition.
4. A man gave the lecture on modern art.
5. Ms. Itakura and the lecturer on proper study habits spoke on 
   consecutive days, in one order or the other.
6. Haller gave a lecture sometime after Eddie did.
7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
'''



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def lecture_series():

  n = 5
  days = range(n)
  Monday, Tuesday, Wednesday, Thursday, Friday = days

  # variables
  lectures = intvar(0,n-1,shape=n,name="lectures")
  choosing_a_college, physical_hygiene, modern_art, nutrition, study_habits = lectures


  first_name = intvar(0,n-1,shape=n,name="first_name")
  Alice, Bernadette, Charles, Duane, Eddie = first_name

  last_name = intvar(0,n-1,shape=n,name="last_name")
  Felicidad, Garber, Haller, Itakura, Jeffreys = last_name

  # constraints
  model = Model([AllDifferent(lectures),
                 AllDifferent(first_name),
                 AllDifferent(last_name),

                 # 1. Alice lectured on Monday.
                 Alice == Monday,

                 # 2. Charles's lecture on physical hygiene wasn't given on Friday.
                 Charles == physical_hygiene,
                 Charles != Friday,
                 physical_hygiene != Friday,
                 
                 # 3. Dietician Jeffreys gave the lecture on nutrition.
                 Jeffreys == nutrition,

                 # 4. A man gave the lecture on modern art.
                 ( (modern_art == Charles) | 
                   (modern_art == Duane)   |
                   (modern_art == Eddie)
                   ),
                 
                 # 5. Ms. Itakura and the lecturer on proper study habits spoke on 
                 #    consecutive days, in one order or the other.
                 ((Itakura == Alice) | (Itakura == Bernadette)),
                 
                 abs(Itakura - study_habits) == 1,
                 Itakura != study_habits,
                 
                 # 6. Haller gave a lecture sometime after Eddie did.
                 Haller > Eddie,
                 
                 # 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
                 Duane == Felicidad,
                 Duane < modern_art,
                 ]
                 )

  def print_sol():
      print("days      :", list(days))
      print("lectures  :", lectures.value())
      print("first_name:", first_name.value())
      print("last_name :", last_name.value())
      print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


lecture_series()
