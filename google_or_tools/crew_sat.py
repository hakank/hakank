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

  Crew allocation problem  in OR-tools CP-SAT Solver.

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

  This is a port of my old CP model crew.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main(sols=1):

  model = cp.CpModel()

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
  crew = {}
  for i in range(num_flights):
    for j in range(num_persons):
      crew[(i, j)] = model.NewIntVar(0, 1, "crew[%i,%i]" % (i, j))
  crew_flat = [
      crew[(i, j)] for i in range(num_flights) for j in range(num_persons)
  ]

  # number of working persons
  num_working = model.NewIntVar(1, num_persons, "num_working")

  #
  # constraints
  #

  # number of working persons
  # Old:
  # model.Add(num_working == sum([
  #     solver.IsGreaterOrEqualCstVar(
  #         solver.Sum([crew[(f, p)]
  #                     for f in range(num_flights)]), 1)
  #     for p in range(num_persons)
  # ]))
  wb = [model.NewBoolVar(f"b[{p}]") for p in range(num_persons)]
  for p in range(num_persons):
    working = model.NewIntVar(0,num_flights,"")
    model.Add(working == sum([crew[(f, p)]
                      for f in range(num_flights)]))
    model.Add(working > 0).OnlyEnforceIf(wb[p])
    model.Add(working == 0).OnlyEnforceIf(wb[p].Not())

  model.Add(num_working == sum(wb))

  for f in range(num_flights):
    # size of crew
    tmp = [crew[(f, i)] for i in range(num_persons)]
    model.Add(sum(tmp) == required_crew[f][0])

    # attributes and requirements
    for j in range(5):
      tmp = [attributes[i][j] * crew[(f, i)] for i in range(num_persons)]
      model.Add(sum(tmp) >= required_crew[f][j + 1])

  # after a flight, break for at least two flights
  for f in range(num_flights - 2):
    for i in range(num_persons):
      model.Add(crew[f, i] + crew[f + 1, i] + crew[f + 2, i] <= 1)

  # extra contraint: all must work at least two of the flights
  # for i in range(num_persons):
  #    [model.Add(sum([crew[f,i] for f in range(num_flights)]) >= 2) ]

  # Minimize the number of working people
  model.Minimize(num_working)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  #
  # result
  #
  if status == cp.OPTIMAL:
    print("Number working:", solver.Value(num_working))
    for i in range(num_flights):
      for j in range(num_persons):
        print(solver.Value(crew[i, j]), end=" ")
      print()
    print()

    print("Flights:")
    for flight in range(num_flights):
      print("Flight", flight, "persons:", end=" ")
      for person in range(num_persons):
        if solver.Value(crew[flight, person]) == 1:
          print(names[person], end=" ")
      print()
    print()

    print("Crew:")
    for person in range(num_persons):
      print("%-10s flights" % names[person], end=" ")
      for flight in range(num_flights):
        if solver.Value(crew[flight, person]) == 1:
          print(flight, end=" ")
      print()
    print()


  print()
  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


num_solutions_to_show = 1
if __name__ == "__main__":
  if (len(sys.argv) > 1):
    num_solutions_to_show = int(sys.argv[1])

  main(num_solutions_to_show)
