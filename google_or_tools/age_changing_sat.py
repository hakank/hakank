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
  
  Enigma 1224 - Age-changing in OR-tools CP-SAT Solver.

  https://enigmaticcode.wordpress.com/2015/06/20/enigma-1224-age-changing/
  '''
  From New Scientist #2380, 1st February 2003

    If you start with my age, in years, and apply the four operations:

    [  
       +2  /8 

       -3  *7

    ]

    in some order, then the final answer you get is my husband’s age in years.

    Funnily enough, if you start with his age and apply the same four operations in a 
    different order, then you get my age.

    What are our two ages?
  '''

  There are two solutions:

    m: 53 h: 48
    hlist: [53, 371, 368, 46, 48]
    mlist: [48, 6, 8, 56, 53]
    perm1: ['*7', '-3', '/8', '+2']
    perm2: ['/8', '+2', '*7', '-3']

    m: 48 h: 53
    hlist: [48, 6, 8, 56, 53]
    mlist: [53, 371, 368, 46, 48]
    perm1: ['/8', '+2', '*7', '-3']
    perm2: ['*7', '-3', '/8', '+2']



  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n,perms,m,h,hlist,mlist,perm1,perm2):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__perms = perms
        self.__m = m 
        self.__h = h
        self.__hlist = hlist
        self.__mlist = mlist
        self.__perm1 = perm1
        self.__perm2 = perm2

        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        n = self.__n
        perms = self.__perms
        m = self.__m
        h = self.__h
        hlist = self.__hlist
        mlist = self.__mlist
        perm1 = self.__perm1
        perm2 = self.__perm2
        
        print("m:", self.Value(m), "h:",self.Value(h))        
        print("hlist:", [self.Value(hlist[i]) for i in range(n+1)])
        print("mlist:", [self.Value(mlist[i]) for i in range(n+1)])
        print("perm1:", [perms[self.Value(perm1[i])] for i in range(n)])
        print("perm2:", [perms[self.Value(perm2[i])] for i in range(n)])
        print()

    def SolutionCount(self):
        return self.__solution_count


def check(model,perm, old, new):
  """
  Connect the permutation `perm` with the operation

  perm == 0 => new == old + 2

  perm == 1 => new == old / 8

  perm == 2 => new == old - 3

  perm == 3 => new == old * 7
  """  
  perm_b = [model.NewBoolVar("perm_b") for i in range(4)]
  for i in range(4):
    model.Add(perm == i).OnlyEnforceIf(perm_b[i])
    model.Add(perm != i).OnlyEnforceIf(perm_b[i].Not())

  eq_b   = [model.NewBoolVar("eq_b") for i in range(4)]
  model.Add(new == old + 2).OnlyEnforceIf(eq_b[0])

  # new = old / 8
  div_val = model.NewIntVar(0,100,"div_val")
  model.AddDivisionEquality(div_val,old, 8)
  model.Add(new == div_val).OnlyEnforceIf(eq_b[1])
  # Checking the division
  model.Add(div_val * 8 == old).OnlyEnforceIf(eq_b[1])

  model.Add(new == old - 3).OnlyEnforceIf(eq_b[2])
  model.Add(new == old * 7).OnlyEnforceIf(eq_b[3])

  for i in range(4):
    model.AddImplication(perm_b[i], eq_b[i])



def main():

  model = cp.CpModel()

  n = 4

  perms = ["+2","/8","-3","*7"]

  # ages 16..120
  age_low = 16
  age_high = 120

  # variables
  m = model.NewIntVar(age_low, age_high,"m") # my age
  h = model.NewIntVar(age_low, age_high,"h") # my age

  perm1 = [model.NewIntVar(0,n-1,"perm1") for i in range(n)]
  perm2 = [model.NewIntVar(0,n-1,"perm2") for i in range(n)]

  # for calculating my age 
  mlist = [model.NewIntVar(0,1000,"perm2") for i in range(n+1)] 
  # for calculating husbands age 
  hlist = [model.NewIntVar(0,1000,"perm2") for i in range(n+1)] 

  # constraints
  model.AddAllDifferent(perm1)
  model.AddAllDifferent(perm2)

  # same operations in different order
  pb = [model.NewBoolVar("pb") for i in range(n)]
  for i in range(n):
    model.Add(perm1[i] != perm2[i]).OnlyEnforceIf(pb[i])
    model.Add(perm1[i] == perm2[i]).OnlyEnforceIf(pb[i].Not())
  model.Add(sum(pb) > 0)

  # find husbands age, start with my age
  model.Add(hlist[0] == m)

  # husband's age is last in hlist
  model.Add(h == hlist[n])

  # checking my age, start with husband's age
  model.Add(mlist[0] == h)

  # my age is last in mlist
  model.Add(m == mlist[n])

  # check the operations
  for i in range(n):
    check(model,perm1[i], hlist[i], hlist[i+1])
    check(model,perm2[i], mlist[i], mlist[i+1])

  # Symmetry breaking: I'm younger than husband
  # model.Add(m < h)

  solver  = cp.CpSolver()
  
  solution_printer = SolutionPrinter(n,perms,m,h,hlist,mlist,perm1,perm2)
  status = solver.SearchForAllSolutions(model, solution_printer)
  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


if __name__ == '__main__':
  main()
