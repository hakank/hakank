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

  Generic alphametic solver in OR-tools CP-SAT Solver.

  This is a generic alphametic solver.

  Usage:
     python3 alphametic.py
                         ->  solves SEND+MORE=MONEY in base 10

     python3 alphametic.py  'SEND+MOST=MONEY' 11
                         -> solves SEND+MOST=MONEY in base 11

     python3 alphametic.py TEST <base>
                         -> solves some test problems in base <base>
                            (defined in test_problems())

  Assumptions:
  - the model only solves problems of the form
           NUMBER<1>+NUMBER<2>...+NUMBER<N-1> = NUMBER<N>
    i.e. the last number is the sum
  - the only nonletter characters are: +, =, \d (which are splitted upon)

  This is a port of my old CP model alphametic.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google OR-tools CP-SAT models:
  http://www.hakank.org/or_tools/

"""
from __future__ import print_function
import sys
import re
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionPrinter


def main(problem_str="SEND+MORE=MONEY", base=10):

  model = cp.CpModel()

  # data
  print("\nproblem:", problem_str)
  print("base:", base)

  # convert to array.
  problem = re.split("[\s+=]", problem_str)
  p_len = len(problem)
  
  # create the lookup table: list of (digit : ix)
  a = sorted(set("".join(problem)))
  n = len(a)
  lookup = dict(list(zip(a, list(range(n)))))

  # length of each number
  lens = list(map(len, problem))

  #
  # declare variables
  #

  # the digits
  x = [model.NewIntVar(0, base - 1, "x[%i]" % i) for i in range(n)]
  # the sums of each number (e.g. the three numbers SEND, MORE, MONEY)
  sums = [model.NewIntVar(1, base**(lens[i]) - 1, "sums[%i]" % i) for i in range(p_len)]

  #
  # constraints
  #
  model.AddAllDifferent(x)

  ix = 0
  for prob in problem:
    this_len = len(prob)

    # sum all the digits with proper exponents to a number
    model.Add(
        sums[ix] == sum([(base**i) * x[lookup[prob[this_len - i - 1]]]
                                for i in range(this_len)[::-1]]))
    # leading digits must be > 0
    model.Add(x[lookup[prob[0]]] > 0)
    ix += 1

  # the last number is the sum of the previous numbers
  model.Add(sum([sums[i] for i in range(p_len - 1)]) == sums[-1])

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # TODO make a better printer!
  solution_printer = SimpleSolutionPrinter(x) 
  status = solver.SearchForAllSolutions(model, solution_printer)

  print("status:",solver.StatusName(status))
  if status == cp.OPTIMAL:
    print("\nLast solution:")
    for i in range(n):
      print(a[i], "=", solver.Value(x[i]))
    print()
    for prob in problem:
      for p in prob:
        print(p, end=" ")
      print()
    print()
    for prob in problem:
      for p in prob:
        print(solver.Value(x[lookup[p]]), end=" ")
      print()

    print("sums:", [solver.Value(sums[i]) for i in range(p_len)])
    print()

  print('NumSolutions:', solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches :", solver.NumBranches())
  print("WallTime    :", solver.WallTime())


def test_problems(base=10):
  problems = [
      "SEND+MORE=MONEY", "SEND+MOST=MONEY", "VINGT+CINQ+CINQ=TRENTE",
      "EIN+EIN+EIN+EIN=VIER", "DONALD+GERALD=ROBERT",
      "SATURN+URANUS+NEPTUNE+PLUTO+PLANETS", "WRONG+WRONG=RIGHT"
  ]

  for p in problems:
    main(p, base)


problem = "SEND+MORE=MONEY"
base = 10
if __name__ == "__main__":
  if len(sys.argv) > 1:
    problem = sys.argv[1]
  if len(sys.argv) > 2:
    base = int(sys.argv[2])

  if problem == "TEST" or problem == "test":
    test_problems(base)
  else:
    main(problem, base)
