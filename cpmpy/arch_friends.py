"""
Arch Friends puzzle (Dells Logic Puzzles) in cpmpy.

From http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
'''
Title: Arch Friends
Author: Mark T. Zegarelli
Publication: Dell Logic Puzzles
Issue: April, 1998
Page: 7
Stars: 1

Harriet, upon returning from the mall, is happily describing her four
shoe purchases to her friend Aurora. Aurora just loves the four
different kinds of shoes that Harriet bought 
  (ecru espadrilles,fuchsia flats, purple pumps, and suede sandals),
but Harriet can't recall at which different store 
  (Foot Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) 
she got each pair. Can you help these two figure out the order in
which Harriet bought each pair of shoes, and where she bought each?

1. Harriet bought fuchsia flats at Heels in a Handcart.
2. The store she visited just after buying her purple pumps was not
  Tootsies.
3. The Foot Farm was Harriet's second stop.
4. Two stops after leaving The Shoe Place, Harriet bought her suede
  sandals.

Determine: Order - Shoes - Store 
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def arch_friends():

  n = 4
  model = Model()

  shoes = intvar(1,n,shape=n,name="shoes")
  ecruespadrilles, fuchsiaflats, purplepumps, suedesandals = shoes

  store = intvar(1,n,shape=n,name="store")
  footfarm, heelsinahandcart, theshoepalace, tootsies = store
  
  model += [AllDifferent(shoes),
            AllDifferent(store),

            # 1. Harriet bought fuchsia flats at Heels in a Handcart.
            fuchsiaflats == heelsinahandcart,

            # 2. The store she visited just after buying her purple pumps was not
            #    Tootsies.
            purplepumps + 1 != tootsies,

            # 3. The Foot Farm was Harriet's second stop.
            footfarm == 2,

            # 4. Two stops after leaving The Shoe Place, Harriet bought her suede
            # sandals.
            theshoepalace + 2 == suedesandals
            ]

  def print_sol():
    print("shoes:",shoes.value())
    print("store:",store.value())
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

arch_friends()
