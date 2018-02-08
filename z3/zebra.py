#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Zebra puzzle in Z3
#
# Port of Google or-tools zebra.py:
# https://github.com/google/or-tools/blob/master/examples/python/zebra.py
# (it's not mine :-)
#
# """
# This is the zebra problem as invented by Lewis Caroll.
#
# There are five houses.
# The Englishman lives in the red house.
# The Spaniard owns the dog.
# Coffee is drunk in the green house.
# The Ukrainian drinks tea.
# The green house is immediately to the right of the ivory house.
# The Old Gold smoker owns snails.
# Kools are smoked in the yellow house.
# Milk is drunk in the middle house.
# The Norwegian lives in the first house.
# The man who smokes Chesterfields lives in the house next to the man
#    with the fox.
# Kools are smoked in the house next to the house where the horse is kept.
# The Lucky Strike smoker drinks orange juice.
# The Japanese smokes Parliaments.
# The Norwegian lives next to the blue house.
#
# Who owns a zebra and who drinks water?
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Solver()

  red = makeIntVar(sol, "red", 1, 5)
  green = makeIntVar(sol,"green", 1, 5)
  yellow = makeIntVar(sol,"yellow",1, 5)
  blue = makeIntVar(sol, "blue", 1, 5)
  ivory = makeIntVar(sol, "ivory", 1, 5)

  englishman = makeIntVar(sol, "enlighman", 1, 5)
  spaniard = makeIntVar(sol,"spaniard",1, 5)
  japanese = makeIntVar(sol,"japanese", 1, 5)
  ukrainian = makeIntVar(sol, "ukrainian", 1, 5)
  norwegian = makeIntVar(sol, "norwegian", 1, 5)

  dog = makeIntVar(sol, "dog", 1, 5)
  snails = makeIntVar(sol, "snails", 1, 5)
  fox = makeIntVar(sol, "fox", 1, 5)
  zebra = makeIntVar(sol, "zebra", 1, 5)
  horse = makeIntVar(sol, "horse", 1, 5)

  tea = makeIntVar(sol,"tea", 1, 5)
  coffee = makeIntVar(sol, "coffee", 1, 5)
  water = makeIntVar(sol, "water", 1, 5)
  milk = makeIntVar(sol, "milk", 1, 5)
  fruit_juice = makeIntVar(sol, "fruit juice", 1, 5)

  old_gold = makeIntVar(sol, "old gold", 1, 5)
  kools = makeIntVar(sol, "kools", 1, 5)
  chesterfields = makeIntVar(sol, "chesterfields", 1, 5)
  lucky_strike = makeIntVar(sol, "lucky strike", 1, 5)
  parliaments = makeIntVar(sol, "parliaments", 1, 5)

  sol.add(Distinct([red, green, yellow, blue, ivory]))
  sol.add(Distinct([englishman, spaniard, japanese,
                                  ukrainian, norwegian]))
  sol.add(Distinct([dog, snails, fox, zebra, horse]))
  sol.add(Distinct([tea, coffee, water, milk,
                                  fruit_juice]))
  sol.add(Distinct([parliaments, kools, chesterfields,
                                  lucky_strike, old_gold]))

  sol.add(englishman == red)
  sol.add(spaniard == dog)
  sol.add(coffee == green)
  sol.add(ukrainian == tea)
  sol.add(green == ivory + 1)
  sol.add(old_gold == snails)
  sol.add(kools == yellow)
  sol.add(milk == 3)
  sol.add(norwegian == 1)
  sol.add(Abs(fox - chesterfields) == 1)
  sol.add(Abs(horse - kools) == 1)
  sol.add(lucky_strike == fruit_juice)
  sol.add(japanese == parliaments)
  sol.add(Abs(norwegian - blue) == 1)

  all_vars = [parliaments, kools, chesterfields, lucky_strike, old_gold,
              englishman, spaniard, japanese, ukrainian, norwegian,
              dog, snails, fox, zebra, horse,
              tea, coffee, water, milk, fruit_juice,
              red, green, yellow, blue, ivory]

  if sol.check() == sat:
    mod = sol.model()
    people = [englishman, spaniard, japanese, ukrainian, norwegian]
    water_drinker = [p for p in people if mod.eval(p) == mod.eval(water)][0]
    zebra_owner = [p for p in people if mod.eval(p) == mod.eval(zebra)][0]
    print('The', water_drinker, 'drinks water.')
    print('The', zebra_owner, 'owns the zebra.')
  else:
    print('No solutions to the zebra problem, this is unusual!')



if __name__ == '__main__':
  main()
