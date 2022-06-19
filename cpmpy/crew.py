"""
Crew allocation problem in cpmpy.

From Gecode example crew
examples/crew.cc
'''
* Example: Airline crew allocation
*
* Assign 20 flight attendants to 10 flights. Each flight needs a certain
* number of cabin crew, and they have to speak certain languages.
* Every cabin crew member has two flights off after an attended flight.
*
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def crew(num_sols=1):

  model = Model()

  #
  # data
  #
  names = [
      "Tom", "David", "Jeremy", "Ron", "Joe", "Bill", "Fred", "Bob", "Mario",
      "Ed", "Carol", "Janet", "Tracy", "Marilyn", "Carolyn", "Cathy", "Inez",
      "Jean", "Heather", "Juliet"
  ]

  num_persons = len(names)  # number of persons

  attributes = [
      #  steward, hostess, french, spanish, german
      [1, 0, 0, 0, 1],  # Tom     = 1
      [1, 0, 0, 0, 0],  # David   = 2
      [1, 0, 0, 0, 1],  # Jeremy  = 3
      [1, 0, 0, 0, 0],  # Ron     = 4
      [1, 0, 0, 1, 0],  # Joe     = 5
      [1, 0, 1, 1, 0],  # Bill    = 6
      [1, 0, 0, 1, 0],  # Fred    = 7
      [1, 0, 0, 0, 0],  # Bob     = 8
      [1, 0, 0, 1, 1],  # Mario   = 9
      [1, 0, 0, 0, 0],  # Ed      = 10
      [0, 1, 0, 0, 0],  # Carol   = 11
      [0, 1, 0, 0, 0],  # Janet   = 12
      [0, 1, 0, 0, 0],  # Tracy   = 13
      [0, 1, 0, 1, 1],  # Marilyn = 14
      [0, 1, 0, 0, 0],  # Carolyn = 15
      [0, 1, 0, 0, 0],  # Cathy   = 16
      [0, 1, 1, 1, 1],  # Inez    = 17
      [0, 1, 1, 0, 0],  # Jean    = 18
      [0, 1, 0, 1, 1],  # Heather = 19
      [0, 1, 1, 0, 0]  # Juliet  = 20
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
  crew = boolvar(shape=(num_flights,num_persons), name="crew")

  # number of working persons
  num_working = intvar(1,num_persons, name="num_working")

  #
  # constraints
  #

  # number of working persons
  model += [num_working == sum([sum([crew[(f, p)] for f in range(num_flights)]) >= 1
                                for p in range(num_persons)])]

  for f in range(num_flights):
    # size of crew
    tmp = [crew[(f, i)] for i in range(num_persons)]
    model += [sum(tmp) == required_crew[f][0]]

    # attributes and requirements
    for j in range(5):
      tmp = [attributes[i][j] * crew[(f, i)] for i in range(num_persons)]
      model += [sum(tmp) >= required_crew[f][j + 1]]

  # after a flight, break for at least two flights
  for f in range(num_flights - 2):
    for i in range(num_persons):
      model += [crew[f, i] + crew[f + 1, i] + crew[f + 2, i] <= 1]

  # extra contraint: all must work at least two of the flights
  # for i in range(num_persons):
  #     model += [sum([crew[f,i] for f in range(num_flights)]) >= 2]

  def print_sol():
    print("Number working:", num_working.value())
    for i in range(num_flights):
      for j in range(num_persons):
        print(crew[i, j].value(), end=" ")
      print()
    print()

    print("Flights:")
    for flight in range(num_flights):
      print("Flight", flight, "persons:", end=" ")
      for person in range(num_persons):
        if crew[flight, person].value() == 1:
          print(names[person], end=" ")
      print()
    print()

    print("Crew:")
    for person in range(num_persons):
      print("%-10s flights" % names[person], end=" ")
      for flight in range(num_flights):
        if crew[flight, person].value() == 1:
          print(flight, end=" ")
      print()
    print()


  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
  print()
  print("num_solutions:", num_solutions)

num_solutions_to_show = 1
if (len(sys.argv) > 1):
  num_solutions_to_show = int(sys.argv[1])

crew(num_solutions_to_show)
