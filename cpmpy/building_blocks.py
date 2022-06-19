"""
Building Blocks puzzle (Dell Logic Puzzles) in cpmpy.

From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
'''
Each of four alphabet blocks has a single letter of the alphabet on each 
of its six sides. In all, the four blocks contain every letter but 
Q and Z. By arranging the blocks in various ways, you can spell all of 
the words listed below. Can you figure out how the letters are arranged 
on the four blocks?

 BAKE ONYX ECHO OVAL
 GIRD SMUG JUMP TORN
 LUCK VINY LUSH WRAP
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def building_blocks():
  
  n = 4
  num_words = 12
  m = 24

  # Index 1 based (adjusted below)
  A = 1;B = 2;C = 3;D = 4;E = 5;F = 6;G = 7;H = 8;I = 9;J = 10;
  K = 11;L = 12;M = 13;N = 14;O = 15;P = 16;R = 17;S = 18;T = 19;
  U = 20;V = 21;W = 22;X = 23;Y = 24;

  alpha = ["A","B","C","D","E","F","G","H","I","J","K","L","M",
           "N","O","P","R","S","T","U","V","W","X","Y"];

  words = np.array([[B,A,K,E],
                    [O,N,Y,X],
                    [E,C,H,O],
                    [O,V,A,L],
                    [G,I,R,D],
                    [S,M,U,G],
                    [J,U,M,P],
                    [T,O,R,N],
                    [L,U,C,K],
                    [V,I,N,Y],
                    [L,U,S,H],
                    [W,R,A,P]])

  dice = intvar(0,n-1,shape=m,name="dice")

  model = Model()
  
  # the letters in a word must be on a different die
  for word in range(num_words):
    # Also, adjust for 1-based
    model += (AllDifferent([dice[words[word]-1]]))
  
  # there must be exactly 6 letters of each die
  model += (global_cardinality_count(dice, [6 for i in range(n)]))

  # symmetry breaking (first word is placed)
  model += (dice[ 0] <= dice[ 6])
  model += (dice[ 6] <= dice[12])

  def print_sol():
    dice_val = dice.value()
    print("dice:", dice_val)
    for i in range(n):
      print(f"die {i}:", end=" ")
      for j in range(m):
        if dice_val[j] == i:
          print(alpha[j],end="")
      print()
    print()
   

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  num_solutions = ss.solveAll(display=print_sol)   
  print("num_solutions:",num_solutions)


building_blocks()
