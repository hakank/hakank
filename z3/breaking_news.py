#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Breaking News puzzle (Dell Logic Puzzles) in Z3
# Problem from  
# http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
# """
# Title: Breaking News
# Author: Faith Johnson
# Publication: Dell Logic Puzzles
# Issue: April, 1998
# Page: 9
# Stars: 1
#
# The Daily Galaxy sent its four best reporters (Corey, Jimmy, Lois, and 
# Perry) to different locations (Bayonne, New Hope, Port Charles, and 
# South Amboy) to cover four breaking news events (30-pound baby, blimp 
# launching, skyscraper dedication, and beached whale). Their editor is 
# trying to remember where each of the reporters is. Can you match the name 
# of each reporter with the place he or she was sent, and the event that 
# each covered?
# 
# 1. The 30-pound baby wasn't born in South Amboy or New Hope.
# 2. Jimmy didn't go to Port Charles.
# 3. The blimp launching and the skyscraper dedication were covered, in some 
#    order, by Lois and the reporter who was sent to Port Charles.
# 4. South Amboy was not the site of either the beached whale or the 
#    skyscraper dedication.
# 5. Bayonne is either the place that Corey went or the place where the 
#    whale was beached, or both.
# 
# Determine: Reporter -- Location -- Story
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

#
# data
#
n = 4

reporters = range(n)
[Corey, Jimmy, Lois, Perry] = reporters

#
# variables
#
locations = makeIntArrayVector(sol,"locations",n,0,n-1)
[Bayonne, New_Hope, Port_Charles, South_Amboy] = locations

events = makeIntArrayVector(sol,"events",n,0,n-1)
[baby, blimp, skyscraper, whale] = events

#
# constraints
#
sol.add(Distinct(locations))
sol.add(Distinct(events))

# 1. The 30-pound baby wasn't born in South Amboy or New Hope.
sol.add(baby != South_Amboy, baby != New_Hope)
 
#  2. Jimmy didn't go to Port Charles.
sol.add(Jimmy != Port_Charles)

#  3. The blimp launching and the skyscraper dedication were covered, 
#     in some order, by Lois and the reporter who was sent to 
#     Port Charles.
sol.add(Or(
     And(blimp == Lois, skyscraper == Port_Charles)
     ,
     And(skyscraper == Lois, blimp == Port_Charles)
   ))

#  4. South Amboy was not the site of either the beached whale or the 
#     skyscraper dedication.
sol.add(South_Amboy != whale, South_Amboy != skyscraper)

#  5. Bayonne is either the place that Corey went or the place where 
#     the whale was beached, or both.
sol.add( If(Bayonne == Corey,1,0) + If(Bayonne == whale,1,0)  >= 1)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("reporters:", [reporters[i] for i in range(n)])
  print("locations:", [mod.eval(locations[i]) for i in range(n)])
  print("events:", [mod.eval(events[i]) for i in range(n)])
  getDifferentSolution(sol,mod,locations,events)

print("num_solutions:", num_solutions)



