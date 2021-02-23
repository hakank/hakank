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

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) 
  in OR-tools CP.SAT Solver.

  This is a standard benchmark for theorem proving.

  http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
  '''
  Someone in Dreadsbury Mansion killed Aunt Agatha.
  Agatha, the butler, and Charles live in Dreadsbury Mansion, and
  are the only ones to live there. A killer always hates, and is no
  richer than his victim. Charles hates noone that Agatha hates. Agatha
  hates everybody except the butler. The butler hates everyone not richer
  than Aunt Agatha. The butler hates everyone whom Agatha hates.
  Noone hates everyone. Who killed Agatha?
  '''

  Originally from F. J. Pelletier:
  Seventy-five problems for testing automatic theorem provers.
  Journal of Automated Reasoning, 2: 216, 1986.

  Note1: Since Google CP Solver/Pythons (currently) don't have
         special support for logical operations on decision
         variables (i.e. ->, <->, and, or, etc), this model
         use some IP modeling tricks.

  Note2: There are 8 different solutions, all stating that Agatha
         killed herself

  This is a port of my old CP model who_killed_agatha.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter
from collections import defaultdict

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""

    def __init__(self, n, the_killer, the_victim, hates_flat, richer_flat,killers):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__the_killer = the_killer
        self.__the_victim = the_victim
        self.__hates_flat = hates_flat
        self.__richer_flat = richer_flat
        self.__killers = killers
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        n = self.__n
        print("the_killer:", self.Value(self.__the_killer))
        self.__killers[self.Value(self.__the_killer)] += 1
        print("the_victim:", self.Value(self.__the_victim))
        print("hates:")
        print_flat_matrix(self,self.__hates_flat, n, n)
        print("richer:")
        print_flat_matrix(self,self.__richer_flat, n, n)
        print()

    def SolutionCount(self):
        return self.__solution_count


def var_matrix_array(model, rows, cols, lb, ub, name):
  x = []
  for i in range(rows):
    t = []
    for j in range(cols):
      t.append(model.NewIntVar(lb, ub, "%s[%i,%i]" % (name, i, j)))
    x.append(t)
  return x


def flatten_matrix(solver, m, rows, cols):
  return [m[i][j] for i in range(rows) for j in range(cols)]


def print_flat_matrix(solver,m_flat, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print(solver.Value(m_flat[i * cols + j]), end=" ")
    print()
  print()


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 3
  agatha = 0
  butler = 1
  charles = 2

  #
  # declare variables
  #
  the_killer = model.NewIntVar(0, 2, "the_killer")
  the_victim = model.NewIntVar(0, 2, "the_victim")

  hates = var_matrix_array(model, n, n, 0, 1, "hates")
  richer = var_matrix_array(model, n, n, 0, 1, "richer")

  hates_flat = flatten_matrix(model, hates, n, n)
  richer_flat = flatten_matrix(model, richer, n, n)

  #
  # constraints
  #

  # Agatha, the butler, and Charles live in Dreadsbury Mansion, and
  # are the only ones to live there.

  # A killer always hates, and is no richer than his victim.
  # solver.Add(hates[the_killer, the_victim] == 1)
  # model.Add(solver.Element(hates_flat, the_killer * n + the_victim) == 1)
  killer_ix = model.NewIntVar(0,n,"the_killer_ix")
  model.Add(killer_ix == the_killer * n + the_victim)
  model.AddElement(killer_ix, hates_flat,1)

  # solver.Add(richer[the_killer, the_victim] == 0)
  # model.Add(solver.Element(richer_flat, the_killer * n + the_victim) == 0)
  model.AddElement(killer_ix, richer_flat,0)

  # define the concept of richer: no one is richer than him-/herself
  for i in range(n):
    model.Add(richer[i][i] == 0)

  # (contd...) if i is richer than j then j is not richer than i
  #  (i != j) => (richer[i,j] = 1) <=> (richer[j,i] = 0),
  for i in range(n):
    for j in range(n):
      if i != j:
        # model.Add((richer[i][j] == 1) == (richer[j][i] == 0))
        b1 = model.NewBoolVar("b1")
        model.Add(richer[i][j] == 1).OnlyEnforceIf(b1)
        model.Add(richer[i][j] != 1).OnlyEnforceIf(b1.Not())
        b2 = model.NewBoolVar("b2")
        model.Add(richer[j][i] == 0).OnlyEnforceIf(b2)
        model.Add(richer[j][i] != 0).OnlyEnforceIf(b2.Not())
        model.Add(b1 == b2)


  # Charles hates noone that Agatha hates.
  # forall i : Range .
  #  (hates[agatha, i] = 1) => (hates[charles, i] = 0),
  for i in range(n):
    # model.Add((hates[agatha][i] == 1) <= (hates[charles][i] == 0))
    b1 = model.NewBoolVar("b1")
    b2 = model.NewBoolVar("b2")
    model.Add(hates[agatha][i] == 1).OnlyEnforceIf(b1)
    model.Add(hates[agatha][i] != 1).OnlyEnforceIf(b1.Not())
    model.Add(hates[charles][i] == 0).OnlyEnforceIf(b2)
    model.Add(hates[charles][i] != 0).OnlyEnforceIf(b2.Not())
    # model.Add(b1 <= b2)
    model.AddImplication(b1,b2)

  # Agatha hates everybody except the butler.
  model.Add(hates[agatha][charles] == 1)
  model.Add(hates[agatha][agatha] == 1)
  model.Add(hates[agatha][butler] == 0)

  # The butler hates everyone not richer than Aunt Agatha.
  # forall i : Range .
  #  (richer[i, agatha] = 0) => (hates[butler, i] = 1),
  for i in range(n):
    # model.Add((richer[i][agatha] == 0) <= (hates[butler][i] == 1))
    b1 = model.NewBoolVar("b1")
    b2 = model.NewBoolVar("b2")
    model.Add(richer[i][agatha] == 0).OnlyEnforceIf(b1)
    model.Add(richer[i][agatha] != 0).OnlyEnforceIf(b1.Not())
    model.Add(hates[butler][i] == 1).OnlyEnforceIf(b2)
    model.Add(hates[butler][i] != 1).OnlyEnforceIf(b2.Not())
    # model.Add(b1 <= b2)
    model.AddImplication(b1,b2)

  # The butler hates everyone whom Agatha hates.
  # forall i : Range .
  #  (hates[agatha, i] = 1) => (hates[butler, i] = 1),
  for i in range(n):
    # model.Add((hates[agatha][i] == 1) <= (hates[butler][i] == 1))
    b1 = model.NewBoolVar("b1")
    b2 = model.NewBoolVar("b2")
    model.Add(hates[agatha][i] == 1).OnlyEnforceIf(b1)
    model.Add(hates[agatha][i] != 1).OnlyEnforceIf(b1.Not())
    model.Add(hates[butler][i] == 1).OnlyEnforceIf(b2)
    model.Add(hates[butler][i] != 1).OnlyEnforceIf(b2.Not())
    # model.Add(b1 <= b2)
    model.AddImplication(b1,b2)


  # Noone hates everyone.
  # forall i : Range .
  #   (sum j : Range . hates[i,j]) <= 2,
  for i in range(n):
    model.Add(sum([hates[i][j] for j in range(n)]) <= 2)

  # Who killed Agatha?
  model.Add(the_victim == agatha)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  killers = defaultdict(int)
  solution_printer = SolutionPrinter(n, the_killer, the_victim, hates_flat, richer_flat, killers)
  status = solver.SearchForAllSolutions(model,solution_printer)
  
  if status != cp.OPTIMAL:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())

  return killers

p = ["agatha", "butler", "charles"]
if __name__ == "__main__":
  killers = main()
  print()
  for k in killers:
    print("The killer <%s> was choosen in %i solutions" % (p[k], killers[k]))
  print()
