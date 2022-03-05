#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Arch friends puzzle (Dell Logic Puzzles)  in Z3
# 
# Problem formulation from 
# http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
# """
# Title: Arch Friends
# Author: Mark T. Zegarelli
# Publication: Dell Logic Puzzles
# Issue: April, 1998
# Page: 7
# Stars: 1
#
# Harriet, upon returning from the mall, is happily describing her four shoe 
# purchases to her friend Aurora. Aurora just loves the four different kinds 
# of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
# and suede sandals), but Harriet can't recall at which different store (Foot 
# Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
# Can you help these two figure out the order in which Harriet bought each 
# pair of shoes, and where she bought each?
#
# 1. Harriet bought fuchsia flats at Heels in a Handcart.
# 2. The store she visited just after buying her purple pumps was not Tootsies.
# 3. The Foot Farm was Harriet's second stop.
# 4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
# 
# Determine: Order - Shoes - Store 
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SimpleSolver()

n = 4

[ecru_espadrilles,fuchsia_flats,purple_pumps,suede_sandals] = makeIntArrayVector(sol,"shoes",n,1,n)
shoes = [ecru_espadrilles,fuchsia_flats,purple_pumps,suede_sandals]

[Foot_Farm,Heels_in_a_Handcart,The_Shoe_Palace,Tootsies] = makeIntArrayVector(sol,"shops",n,1,n)
shops = [Foot_Farm,Heels_in_a_Handcart,The_Shoe_Palace,Tootsies]

sol.add(Distinct(shoes))
sol.add(Distinct(shops))

# 1. Harriet bought fuchsia flats at Heels in a Handcart.
sol.add(fuchsia_flats == Heels_in_a_Handcart)

# 2. The store she visited just after buying her purple pumps was not Tootsies.
sol.add(purple_pumps + 1 != Tootsies)

# 3. The Foot Farm was Harriet's second stop.
sol.add(Foot_Farm == 2)

# 4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
sol.add(The_Shoe_Palace + 2 == suede_sandals)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("shoes:", [mod.eval(shoes[i]) for i in range(n)])
  print("shops:", [mod.eval(shops[i]) for i in range(n)])
  getDifferentSolution(sol,mod,shoes,shops)
  
print("num_solutions:", num_solutions)

