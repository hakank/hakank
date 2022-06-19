"""
Allergy problem in cpmpy

This is a port of the xcsp model Allergy.py
'''
Four friends (two women named Debra and Janet, and two men named Hugh and Rick) found
that each of them is allergic to something different:
eggs, mold, nuts and ragweed.
We would like to match each one's surname (Baxter, Lemon, Malone and Fleet) with his or her allergy.
We know that:
 - Rick is not allergic to mold
 - Baxter is allergic to eggs
 - Hugh is neither surnamed Lemon nor Fleet
 - Debra is allergic to ragweed
 - Janet (who isn't Lemon) is neither allergic to eggs nor to mold
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from collections import defaultdict


def allergy():

  n = 4
  friends = Debra, Janet, Hugh, Rick = list(range(n))
  friends_s = ["Debra", "Janet", "Hugh", "Rick"]

  # foods[i] is the friend allergic to the ith food
  eggs, mold, nuts, ragweed = foods = intvar(0,n-1,shape=n,name="foods")
  foods_s = ["eggs", "mold", "nuts", "ragweed"]

  # surnames[i] is the friend with the ith surname
  baxter, lemon, malone, fleet = surnames = intvar(0,n-1,shape=n, name="surnames")
  surnames_s = ["baxter", "lemon","malone", "fleet"]

  model = Model([AllDifferent(foods),
                 AllDifferent(surnames),

                 mold != Rick,
                 eggs == baxter,
                 lemon != Hugh,
                 fleet != Hugh,
                 ragweed == Debra,
                 lemon != Janet,
                 eggs != Janet,
                 mold != Janet])

  def print_sol():
    print("friends :", [friends_s[friends[i]] for i in range(n)])
    print("foods   :", [(foods_s[i],friends_s[foods[i].value()]) for i in range(n)])
    print("surnames:", [(surnames_s[i],friends_s[surnames[i].value()]) for i in range(n)])
    print()


  ss = CPM_ortools(model)
  num_solutions = model.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

allergy()
