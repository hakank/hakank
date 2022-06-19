"""
Lewis Carrol's classical puzzle with five houses and a zebra:
'''
Five men with different nationalities live in the first five houses
of a street.  They practise five distinct professions, and each of
them has a favourite animal and a favourite drink, all of them
different.  The five houses are painted in different colours.

The Englishman lives in a red house.
The Spaniard owns a dog.
The Japanese is a painter.
The Italian drinks tea.
The Norwegian lives in the first house on the left.
The owner of the green house drinks coffee.
The green house is on the right of the white one.
The sculptor breeds snails.
The diplomat lives in the yellow house.
Milk is drunk in the middle house.
The Norwegian's house is next to the blue one.
The violinist drinks fruit juice.
The fox is in a house next to that of the doctor.
The horse is in a house next to that of the diplomat.

Who owns a Zebra, and who drinks water?
'''
The solution:

The japanese owns the zebra
The norwegian drinks water


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def zebra():
  # There are five houses.
  n = 5
   
  color = intvar(1,n,shape=n,name="color")
  red,green,yellow,blue,ivory = color
  
  nationality = intvar(1,n,shape=n,name="nationality")
  englishman,spaniard,japanese,ukrainian,norwegian = nationality
  # This is used in the solution below.
  englishman.name="englishman"; spaniard.name="spaniard";
  japanese.name="japanese";ukrainian.name="ukrainan";
  norwegian.name="norwegian"
  
  animal = intvar(1,n,shape=n,name="animal")
  dog,snails,fox,zebra,horse = animal
  
  drink = intvar(1,n,shape=n,name="drink")
  tea,coffee,water,milk,fruit_juice = drink
  
  smoke = intvar(1,n,shape=n,name="smoke")        
  old_gold,kools,chesterfields,lucky_strike,parliaments = smoke

  model = Model([AllDifferent(color),
                 AllDifferent(nationality),
                 AllDifferent(animal),
                 AllDifferent(drink),
                 AllDifferent(smoke)]
                 )
  
  # The Englishman lives in the red house.
  model += [englishman == red]
  # The Spaniard owns the dog.
  model += [spaniard == dog]
  # Coffee is drunk in the green house.
  model += [coffee == green]
  # The Ukrainian drinks tea.
  model += [ukrainian == tea]
  # The green house is immediately to the right of the ivory house.
  model += [green == ivory + 1]
  # The Old Gold smoker owns snails.  
  model += [old_gold == snails]
  # Kools are smoked in the yellow house.
  model += [kools == yellow]
  # Milk is drunk in the middle house.
  model += [milk == 3]
  # The Norwegian lives in the first house.  
  model += [norwegian == 1]
  # The man who smokes Chesterfields lives in the house next to the man
  # with the fox.
  # model += [(1 == fox - chesterfields) | (1 == chesterfields - fox)]
  model += [1 == abs(fox - chesterfields) ] 
  # Kools are smoked in the house next to the house where the horse is kept.
  # model += [(1 == horse - kools) | (1 == kools - horse)]
  model += [abs(horse - kools) == 1]
  # The Lucky Strike smoker drinks orange juice.
  model += [lucky_strike == fruit_juice]
  # The Japanese smokes Parliaments.
  model += [japanese == parliaments]
  # The Norwegian lives next to the blue house.
  # model += [(1 == norwegian - blue) | (1 == blue- norwegian )]
  model += [1 == abs(norwegian - blue)]

  print(model)

  def print_sol():
      print("color      :",color.value())
      print("nationality:",nationality.value())
      print("animal     :",animal.value())
      print("drink      :",drink.value())
      print("smoke      :",smoke.value())
      people = [englishman, spaniard, japanese, ukrainian, norwegian]
      water_drinker = [
          p for p in people if p.value() == water.value()][0]
      print(f"The {water_drinker} drinks water")
      zebra_owner = [
          p for p in people if p.value() == zebra.value()][0]
      print(f"The {zebra_owner} owns a zebra")
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  # print("num_solutions:",num_solutions)


zebra()
