"""
 Nontransitive dice in cpmpy.

  From 
  http://en.wikipedia.org/wiki/Nontransitive_dice
  ""
  A set of nontransitive dice is a set of dice for which the relation 
  "is more likely to roll a higher number" is not transitive. See also 
  intransitivity.
  
  This situation is similar to that in the game Rock, Paper, Scissors, 
  in which each element has an advantage over one choice and a 
  disadvantage to the other.
  ""
  
  I start with the 3 dice version, e.g.
  ""
     * die A has sides {2,2,4,4,9,9},
     * die B has sides {1,1,6,6,8,8}, and
     * die C has sides {3,3,5,5,7,7}.
  ""


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def nontransitive_dice(m=3,n=6,given_dice=""):

    # m = 3 # number of dice
    # n = 6 # number of sides of each die

    min_val = 1
    max_val = 6
    if given_dice != "":
        dice_flatten = flatten_lists(given_dice) 
        min_val = min(dice_flatten)
        max_val = max(dice_flatten)        

    # the dice
    # start value might be 0 since Efron's dice requires it.
    dice = intvar(min_val,max_val*2,shape=(m, n), name='dice')
    
    
    # the competitions
    # comp[0,0]: die 0 vs die 1
    # comp[0,1]: die 1 vs die 0
    # comp[1,0]: die 1 vs die 2
    # comp[1,1]: die 2 vs die 1
    # ...
    # comp[m-1,0]: die (m-1) vs die 0
    # comp[m-1,1]: die 0 vs die (m-1)
    comp = intvar(0,n*n,shape=(m, 2), name='comp')

    # probability gap
    gap = intvar(0, n*n,shape=m,name='gap')
    gap_sum = intvar(0,m*n*n, name='gap_sum')
    
    max_val = intvar(0,n*2,name='max_val')
    max_win = intvar(0,n*n,name='max_win')
   
    model = Model (
        max_win == max(comp.flat),
        max_val == max(dice.flat),

        # increasing order of a die
        [ increasing(dice[i]) for i in range(m) ],

        # nontransitivity:
        [ comp[i,0] > comp[i,1] for i in range(m)],

        # probability gap
        [gap[i] == comp[i,0] - comp[i,1] for i in range(m)],
        gap_sum == sum(gap),

    )

    
    if given_dice != "":
        for i in range(m):
            for j in range(n):
                model += [dice[i,j] == given_dice[i][j]]
        
        #
        # Extra constraints to play with
        # 
        # all wins has the same value
        # [ comp[i,0] == comp[i+1,0] for i in range(m-1)],

        # all values of the dice are different
        # AllDifferent(dice),


    # calculate the number of winnings of each round
    # (0 vs 1 and 1 vs 0, 1 vs 2 and 2 vs 1, ... m-1 vs 0 and 0 vs m-1)
    for d in range(m):
        model += [comp[d % m,0] == sum([dice[d % m, r1] > dice[(d+1) % m, r2]
                                       for r1 in range(n) for r2 in range(n)])]
        model += [ comp[d % m,1] == sum([dice[(d+1) % m, r1] > dice[(d) % m, r2]
                                       for r1 in range(n) for r2 in range(n)])]

    # Symmetry breaking: lex_less
    # Note: this don't work for some of the hardcoded examples.
    if given_dice == "":
        model += [lex_less(dice[i],dice[i+1]) for i in range(m-1)]

    def print_sol():
        print("dice:\n", dice.value())       
        print("comp:\n", comp.value())
        print("probabilities:\n", [(comp[i,0].value()/(n*n*1.0),comp[i,1].value()/(n*n*1.0))  for i in range(m)])
        print("gap:", gap.value())
        print("gap_sum:", gap_sum.value())
        print("max_val:", max_val.value())
        print("max_win:", max_win.value())
        print()


    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(solution_limit=1,display=print_sol)
    print("num_solutions:", num_solutions)
    print("status:", ss.status())

#
# Examples of nontransitivity dice.
# 
# Note:
# When running these make sure that other constraints don't
# conflicts with it, e.g. lex_less
#      
dice_examples = {
    # Testing the dice from the Wikipedia page
        # 3 dice
        # dice[0] == [2,2,4,4,9,9], # die A
        # dice[1] == [1,1,6,6,8,8], # die B
        # dice[2] == [3,3,5,5,7,7], # die C
    
    "wikipedia": [ 
        [2,2,4,4,9,9], # die A
        [1,1,6,6,8,8], # die B
        [3,3,5,5,7,7], # die C

    ],

    # Example from Tutorial, page 32 (slide 67/175)
    # dice[0] == [1,2,3,4,5,5], # die A        
    # dice[1] == [3,3,3,3,3,3], # die B
    # dice[2] == [2,2,2,3,6,6], # die C
    "tutorial" : [
        [1,2,3,4,5,5], # die A        
        [3,3,3,3,3,3], # die B
        [2,2,2,3,6,6], # die C
    ],

        # Efron's 4 dice, the number of each die are re-ordered
        # (from the Wikipedia page)
        # dice[0] == [0, 0, 4, 4, 4, 4], #  A
        # dice[1] == [3, 3, 3, 3, 3, 3], #  B        
        # dice[2] == [2, 2, 2, 2, 6, 6], #  C
        # dice[3] == [1, 1, 1, 5, 5, 5], #  D
    "efron" : [
        [0, 0, 4, 4, 4, 4], #  A
        [3, 3, 3, 3, 3, 3], #  B        
        [2, 2, 2, 2, 6, 6], #  C
        [1, 1, 1, 5, 5, 5], #  D
    ],

    # Miwin's dice (3 dice)
    # Miwin's Dice were invented in 1975 by the physicist Michael Winkelmann.
    # (from the Wikipedia page)
    # dice[0] == [1, 2, 5, 6, 7, 9],  # III
    # dice[1] == [1, 3, 4, 5, 8, 9],  # IV
    # dice[2] == [2, 3, 4, 6, 7, 8],  # V
    "mitwin" : [
       [1, 2, 5, 6, 7, 9],  # III
       [1, 3, 4, 5, 8, 9],  # IV
       [2, 3, 4, 6, 7, 8],  # V
    ]
}


num_dice = 3 # number of dice
num_sides = 6 # number of sides of each die
nontransitive_dice(num_dice,num_sides)

#
# Check all instances
#
for p in dice_examples:
    print("\nproblem:", p)
    t = dice_examples[p]
    num_dice = len(t)
    num_sides = len(t[0])
    nontransitive_dice(num_dice,num_sides,t)
