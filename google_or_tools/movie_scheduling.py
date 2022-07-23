# Copyright 2022 James Addison <jay@jp-hosting.net>
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
  Movie scheduling problem in Google CP Solver.

  From Steven Skiena "The Algorithm Design Manual", page 9ff.

  Data from figure 1.5 (with estimated times).

  Movie                      Interval
  -----------------------------------
  Tarjan of the Jungle         4..13
  The Four Volume Problem     17..27
  The President's Algorist     1..10
  Steiner's Tree              12..18
  Process Terminated          23..30
  Halting State                9..16
  Programming Challenges      19..25
  "Discrete" Mathematics       2..7
  Calculated Bets             26..31

  Ported from MiniZinc to Google CP Solver by James Addison, jay@jp-hosting.net
  The MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also his MiniZinc page: http://www.hakank.org/minizinc/
"""
from collections import namedtuple

from ortools.sat.python.cp_model import (
    CpModel,
    CpSolver,
)


Movie = namedtuple("Movie", ["title", "begin", "end"])


if __name__ == "__main__":
    #
    # data
    #
    movies = [
        Movie("Tarjan of the Jungle", 4, 13),
        Movie("The Four Volume Problem", 17, 27),
        Movie("The President's Algorist", 1, 10),
        Movie("Steiner's Tree", 12, 18),
        Movie("Process Terminated", 23, 30),
        Movie("Halting State", 9, 16),
        Movie("Programming Challenges", 19, 25),
        Movie("'Discrete' Mathematics", 2, 7),
        Movie("Calculated Bets", 26, 31),
    ]
    n = len(movies)

    model = CpModel()

    #
    # declare variables
    #
    z = model.NewIntVar(0, n, "z")
    x = [model.NewBoolVar(f"x[{i}]") for i in range(n)]

    #
    # constraints
    #
    for i in range(n):
        for j in range(n):
            if i < j:
                no_overlaps = [
                    movies[i].begin >= movies[j].end,
                    movies[j].begin >= movies[i].end,
                ]
                pair_selected = [x[i], x[j]]
                model.AddBoolOr(no_overlaps).OnlyEnforceIf(pair_selected)

    #
    # goal optimization
    #
    model.Add(z == sum(x))
    model.Maximize(z)

    #
    # solution
    #
    solver = CpSolver()
    solver.Solve(model)

    print(f"z: {solver.Value(z)}")
    print(f"x: {[solver.Value(x[i]) for i in range(n)]}")
    for i in range(n):
        if solver.Value(x[i]):
            print(f'"{movies[i].title}": {movies[i].begin}..{movies[i].end}')
