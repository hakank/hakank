#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Crew allocation problem in Z3
#
# From Gecode example crew
# examples/crew.cc
# '''
# * Example: Airline crew allocation
# *
# * Assign 20 flight attendants to 10 flights. Each flight needs a certain
# * number of cabin crew, and they have to speak certain languages.
# * Every cabin crew member has two flights off after an attended flight.
# *
# '''
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main(sols=1):

  sol = SimpleSolver()

  #
  # data
  #
  names = ["Tom",
           "David",
           "Jeremy",
           "Ron",
           "Joe",
           "Bill",
           "Fred",
           "Bob",
           "Mario",
           "Ed",
           "Carol",
           "Janet",
           "Tracy",
           "Marilyn",
           "Carolyn",
           "Cathy",
           "Inez",
           "Jean",
           "Heather",
           "Juliet"]

  num_persons = len(names)  # number of persons

  attributes = [
      #  steward, hostess, french, spanish, german
      [1, 0, 0, 0, 1],   # Tom     = 1
      [1, 0, 0, 0, 0],   # David   = 2
      [1, 0, 0, 0, 1],   # Jeremy  = 3
      [1, 0, 0, 0, 0],   # Ron     = 4
      [1, 0, 0, 1, 0],   # Joe     = 5
      [1, 0, 1, 1, 0],   # Bill    = 6
      [1, 0, 0, 1, 0],   # Fred    = 7
      [1, 0, 0, 0, 0],   # Bob     = 8
      [1, 0, 0, 1, 1],   # Mario   = 9
      [1, 0, 0, 0, 0],   # Ed      = 10
      [0, 1, 0, 0, 0],   # Carol   = 11
      [0, 1, 0, 0, 0],   # Janet   = 12
      [0, 1, 0, 0, 0],   # Tracy   = 13
      [0, 1, 0, 1, 1],   # Marilyn = 14
      [0, 1, 0, 0, 0],   # Carolyn = 15
      [0, 1, 0, 0, 0],   # Cathy   = 16
      [0, 1, 1, 1, 1],   # Inez    = 17
      [0, 1, 1, 0, 0],   # Jean    = 18
      [0, 1, 0, 1, 1],   # Heather = 19
      [0, 1, 1, 0, 0]    # Juliet  = 20
  ]

  # The columns are in the following order:
  # staff     : Overall number of cabin crew needed
  # stewards  : How many stewards are required
  # hostesses : How many hostesses are required
  # french    : How many French speaking employees are required
  # spanish   : How many Spanish speaking employees are required
  # german    : How many German speaking employees are required
  required_crew = [
      [4, 1, 1, 1, 1, 1],  # Flight 1
      [5, 1, 1, 1, 1, 1],  # Flight 2
      [5, 1, 1, 1, 1, 1],  # ..
      [6, 2, 2, 1, 1, 1],
      [7, 3, 3, 1, 1, 1],
      [4, 1, 1, 1, 1, 1],
      [5, 1, 1, 1, 1, 1],
      [6, 1, 1, 1, 1, 1],
      [6, 2, 2, 1, 1, 1],  # ...
      [7, 3, 3, 1, 1, 1]  # Flight 10
  ]

  num_flights = len(required_crew)  # number of flights

  #
  # declare variables
  #
  crew = {}
  for i in range(num_flights):
    for j in range(num_persons):
      crew[(i, j)] = makeIntVar(sol, "crew[%i,%i]" % (i, j), 0, 1)
  crew_flat = [crew[(i, j)] for i in range(num_flights)
               for j in range(num_persons)]

  # number of working persons
  num_working = makeIntVar(sol, "num_working", 1, num_persons)

  # constraints

  # number of working persons
  sol.add(num_working ==
          Sum([If(Sum([crew[(f, p)] for f in range(num_flights)]) >= 1,1,0) for p in range(num_persons)]))

  for f in range(num_flights):
    # size of crew
    sol.add(Sum([crew[(f, i)] for i in range(num_persons)]) == required_crew[f][0])

    # attributes and requirements
    for j in range(5):
      sol.add(Sum([attributes[i][j] * crew[(f, i)] for i in range(num_persons)]) >= required_crew[f][j + 1])

  # after a flight, break for at least two flights
  for f in range(num_flights - 2):
    for i in range(num_persons):
      sol.add(crew[f, i] + crew[f + 1, i] + crew[f + 2, i] <= 1)

  # extra contraint: all must work at least two of the flights
  for i in range(num_persons):
      [sol.add(Sum([crew[f,i] for f in range(num_flights)]) >= 2) ]

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("Solution #%i" % num_solutions)
    print("Number working:", mod.eval(num_working))
    for i in range(num_flights):
      for j in range(num_persons):
        print(mod.eval(crew[i, j]), end=' ')
      print()
    print()

    print("Flights:")
    for flight in range(num_flights):
      print("Flight", flight, "persons:", end=' ')
      for person in range(num_persons):
        if mod.eval(crew[flight, person]) == 1:
          print(names[person], end=' ')
      print()
    print()

    print("Crew:")
    for person in range(num_persons):
      print("%-10s flights" % names[person], end=' ')
      for flight in range(num_flights):
        if mod.eval(crew[flight, person]) == 1:
          print(flight, end=' ')
      print()
    print()

    if num_solutions >= sols:
      break

  print()
  print("num_solutions:", num_solutions)

num_solutions_to_show = 1
if __name__ == "__main__":
  if (len(sys.argv) > 1):
    num_solutions_to_show = int(sys.argv[1])

  main(num_solutions_to_show)
