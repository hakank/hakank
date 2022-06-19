"""
Tourist site competition in cpmpy

From Pierre Flener's presentation 
'Constraint Technology - A Programming Paradigm on the Rise'
http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
   pages 5f: problem statement 
   pages 12f: model
   pages 21ff: walktrough of a solution

With 7 tourist sites and 7 judges:
'''
Every tourist site is visited by r = 3 judges.
Every judge visits c = 3 tourist sites.
Every pair of sites is visited by lambda = 1 common judge.
'''

There are 151200 solutions to this problem.
With the additional constraint that Ali should visit Birka, Falun and Lund
there are 4320 solutions.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def tourist_site_competition():

  model = Model()

  r = 3
  c = 3
  llambda = 1

  # sites
  num_sites = 7
  sites = range(num_sites)
  [Birka,Falun,Lund,Mora,Sigtuna,Uppsala,Ystad] = sites

  sites_s = ["Birka","Falun","Lund","Mora","Sigtuna","Uppsala","Ystad"]

  # judges
  num_judges = 7
  judges = range(num_judges)
  [Ali,Dan,Eva,Jim,Leo,Mia,Ulla] = judges
  judges_s = ["Ali","Dan","Eva","Jim","Leo","Mia","Ulla"]

  # variables
  x = boolvar(shape=(num_sites,num_judges),name="x")

  # symmetry breaking
  for s in range(3):
    model += [x[(s, 0)] == 1]

  # Every tourist site is visited by r judges.
  for s in range(num_sites):
    model += [r == sum([x[(s,j)] for j in range(num_judges)])]
    
  # Every judge visits c tourist sites.
  for j in range(num_judges):
    model += [c == sum([x[(s,j)] for s in range(num_sites)])]

  # Every pair of sites is visited by lambda common judge.
  for s1 in range(num_sites):
    for s2 in range(num_sites):
      if s1 < s2:
        model += [llambda == sum([(x[s1,j] == 1) & (x[s1,j] == x[s2,j]) for j in range(num_judges)])]

  # where are the judges?
  # for j in range(num_judges):
  #   for s in range(num_sites):
  #     model += [(x[s,j] == 1) == member_of(sol,s,judges_where[j]))

  def print_sol():
    for j in range(num_judges):
      print(judges_s[j], ":",end=" ")
      for s in range(num_sites):
        if x[s,j].value() == 1:
          print(sites_s[s],end=" ")
      print()
    print()
   

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


tourist_site_competition()
