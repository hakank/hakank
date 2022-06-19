"""
Tunalalooza puzzle (Dell Logic Puzzles) in cpmpy.

http://brownbuffalo.sourceforge.net/TunapaloozaClues.html
'''
Title: Tunapalooza
Author: Eliot George
Publication: Dell Logic Puzzles
Issue: April, 1998
Page: 10
Stars: 2

Tim and Keri have a full day ahead for themselves as they plan to see 
and hear everything at Tunapalooza '98, the annual save-the-tuna benefit 
concert in their hometown. To cover the most ground, they will have to 
split up. They have arranged to meet during four rock band acts 
(Ellyfish, Korrupt, Retread Ed and the Flat Tires, and Yellow Reef) at 
planned rendezvous points (carnival games, information booth, mosh pit, 
or T-shirt vendor). Can you help match each band name with the type of 
music they play (country, grunge, reggae, or speed metal) and Tim and 
Kerri's prearranged meeting spot while they play?

1. Korrupt isn't a country or grunge music band.
2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
   performance.
3. The pair won't meet at the T-shirt vendor during the reggae band's show.
4. Exactly two of the following three statements are true:
a) Ellyfish plays grunge music.
b) Tim and Kerri won't meet at the information booth during a 
   performance by Retread Ed and the Flat Tires.
c) The two friends won't meet at the T-shirt vendor while Yellow Reef is playing.
5. The country and speed metal acts are, in some order, Retread Ed 
   and the Flat Tires and the act during which Tim and Kerri will 
   meet at the mosh pit.
6. The reggae band is neither Korrupt nor the act during which Tim and 
   Kerri will meet at the information booth.

Determine: Band name -- Music type -- Meeting place
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def tunapalooza():
  # data
  n = 4
  rockband = list(range(n))
  Ellyfish, Korrupt, Retread_Ed_and_the_Flat_Tires, Yellow_Reef = rockband


  # variables
  genre = intvar(0,n-1,shape=n,name="genre")
  country, grunge, reggae, speed_metal = genre

  rendevouz = intvar(0,n-1,shape=n,name="rendevouz")
  carnival_games, information_booth, mosh_pit, T_shirt_vendor = rendevouz

  # constraints
  model = Model([AllDifferent(genre),
                 AllDifferent(rendevouz),

                 # 1. Korrupt isn't a country or grunge music band.
                 Korrupt != country,Korrupt != grunge,
                 
                 # 2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
                 #    performance.
                 Ellyfish != carnival_games,
                 
                 # 3. The pair won't meet at the T-shirt vendor during the reggae 
                 #    band's show.
                 reggae != T_shirt_vendor,
                 
                 # 4. Exactly two of the following three statements are true:
                 # a) Ellyfish plays grunge music.
                 # b) Tim and Kerri won't meet at the information booth during a 
                 #    performance by Retread Ed and the Flat Tires.
                 # c) The two friends won't meet at the T-shirt vendor while 
                 #    Yellow Reef is playing.
                 (Ellyfish == grunge) +  
                 (information_booth != Retread_Ed_and_the_Flat_Tires) +
                 (T_shirt_vendor != Yellow_Reef) == 2,
                 
                 # 5. The country and speed metal acts are, in some order, Retread Ed 
                 #    and the Flat Tires and the act during which Tim and Kerri will 
                 #    meet at the mosh pit.
                 ((country == Retread_Ed_and_the_Flat_Tires) & (speed_metal == mosh_pit) ) |
                 ((speed_metal == Retread_Ed_and_the_Flat_Tires) & (country == mosh_pit) ),
                 
                 # 6. The reggae band is neither Korrupt nor the act during which Tim and 
                 #    Kerri will meet at the information booth.
                 reggae != Korrupt,
                 reggae != information_booth,
                 ])

  def print_sol():
    print("rockband :", rockband)
    print("genre    :", genre.value())
    print("rendevouz:", rendevouz.value())
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

tunapalooza()


     
