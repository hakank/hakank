"""
Exodus puzzle (Dell Logic Puzzles) in cpmpy.

From 
http://brownbuffalo.sourceforge.net/ExodusClues.html
'''
Title: Exodus
Author: Sophy McHannot
Publication: Dell Logic Puzzles
Issue: April, 1998
Page: 14
Stars: 2

In preparation for Passover, five children at Hebrew school 
(Bernice,Carl,Debby,Sammy, and Ted) 
have been chosen to present
different parts of the story of the Exodus from Egypt 
 (burning bush, captivity,
  Moses's youth, Passover, or the Ten Commandments). 
Each child is a different age 
  (three, five, seven, eight, or ten), 
and the family of each child has recently made its own exodus 
to America from a different country 
(Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
Can you find the age of each child, his or her family's country of 
origin, and the part of the Exodus story each related?

 1. Debby's family is from Lithuania.
 2. The child who told the story of the Passover is two years older
    than Bernice.
 3. The child whose family is from Yemen is younger than the child from
    the Ethiopian family.
 4. The child from the Moroccan family is three years older than Ted.
 5. Sammy is three years older than the child who told the story of
    Moses's youth in the house of the Pharaoh.
 6. Carl related the story of the captivity of the Israelites in Egypt.
 7. The five-year-old child told the story of the Ten Commandments.
 8. The child who told the story of the burning bush is either two or
    three years older than the one whose family came from
    Kazakhstan.

Determine: Age -- Child -- Country -- Story
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def exodus():
  model = Model()
  
  n = 5

  people = list(range(n))
  [Bernice,Carl,Debby,Sammy,Ted] = people

  # variables
  Story = intvar(0,n-1,shape=n,name="Story")
  BurningBush, Captivity, MosessYouth, Passover, TenCommandments = Story

  vals = [3,5,7,8,10]
  Age = intvar(min(vals),max(vals),shape=n,name="Age")
  # Fix the domain
  for i in range(n):
    model += (member_of(vals,Age[i]))


  Country = intvar(0,n-1,shape=n,name="Country")
  Ethiopia, Kazakhstan, Lithuania, Morocco, Yemen = Country

  # constraints
  model += ([AllDifferent(Story),
             AllDifferent(Age),
             AllDifferent(Country),
  
             Debby == Lithuania,
             Age[Passover] == Age[Bernice] + 2,
             Age[Yemen] < Age[Ethiopia],
             Age[Morocco] == Age[Ted] + 3,
             Age[Sammy] == Age[MosessYouth] + 3,
             Carl == Captivity,
             Age[TenCommandments] == 5,
             ((Age[BurningBush] == Age[Kazakhstan] + 2) | (Age[BurningBush] == Age[Kazakhstan] + 3))
             ])

  def print_sol():
    print("People :", people)
    print("Story  :", Story.value())
    print("Country:", Country.value())
    print("Age    :", Age.value())
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

exodus()





