#!/usr/bin/python
"""
 Nontransitive dice in Numberjack.

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
  
  I start with the 3 dice version
  ""
     * die A has sides {2,2,4,4,9,9},
     * die B has sides {1,1,6,6,8,8}, and
     * die C has sides {3,3,5,5,7,7}.
  ""


 3 dice:
 Maximum winning: 27
 comp: [19, 27, 19]
 dice:
 [[0, 0, 3, 6, 6, 6],
  [2, 5, 5, 5, 5, 5],
  [1, 1, 4, 4, 4, 7]]
 max_win: 27
 Number of solutions:  1
 Nodes: 1649873  Time: 25.94
 getFailures: 1649853
 getBacktracks: 1649873
 getPropags: 98105090

 Max winnings where they are the same: 21
 comp: [21, 21, 21]
 dice:
 [[0, 0, 3, 3, 3, 6],
  [2, 2, 2, 2, 2, 5],
  [1, 1, 1, 4, 4, 4]]
 max_win: 21


This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *


def model(libs):

    m = 3 # number of dice
    n = 6 # number of sides of each die

    # the dice
    # start value is 0 since Efron's dice requires it.
    dice = Matrix(m, n, 0, n*2, 'dice')

    # the competitions
    # comp[0,0]: die 0 vs die 1
    # comp[0,1]: die 1 vs die 0
    # comp[1,0]: die 1 vs die 2
    # comp[1,1]: die 2 vs die 1
    # ...
    # comp[m-1,0]: die (m-1) vs die 0
    # comp[m-1,1]: die 0 vs die (m-1)
    comp = Matrix(m, 2, 0, n*n,'comp')

    # probability gap
    gap = VarArray(m, 0, n*n, 'gap')
    gap_sum = Variable(m*n*n, 'gap_sum')
    
    max_val = Variable(0,n*2,'max_val')
    max_win = Variable(0,n*n,'max_win')
    
    model = Model (
        # Optimizations
        #Maximise(max_win),        
        #Minimise(max_val),
        #Maximise(gap_sum),
        #Minimise(gap_sum),        
                
        max_win == Max(comp.flat),
        max_val == Max(dice.flat),

        # Symmetry breaking, lex_greater of the dice
        # This don't work with all test cases, though
        #[ dice.row[i] > dice.row[i+1] for i in range(m-1) ],

        # order of the number of each die, lowest first
        [ (dice[i,j] <= dice[i,j+1]) for i in range(m) for j in range(n-1)],

        # nontransitivity:
        [ comp[i,0] > comp[i,1] for i in range(m)],

        # probability gap
        [gap[i] == comp[i,0] - comp[i,1] for i in range(m)],
        gap_sum == Sum(gap),

        #
        # Extra constraints
        # 
        # all wins has the same value
        # array version
        # [ comp[i,0] == comp[i+1,0] for i in range(m-1)],

        # all values of the dice are different
        #AllDiff(dice.flat),

        #
        # Test cases
        #
        
        # Testing the dice from the Wikipedia page
        # 3 dice
        #dice.row[0] == [2,2,4,4,9,9], # die A
        #dice.row[1] == [1,1,6,6,8,8], # die B
        #dice.row[2] == [3,3,5,5,7,7], # die C

        # Example from Tutorial, page 32 (slide 67/175)
        #dice.row[0] == [1,2,3,4,5,5], # die A        
        #dice.row[1] == [3,3,3,3,3,3], # die B
        #dice.row[2] == [2,2,2,3,6,6], # die C


        # Efron's 4 dice, the number of each die are re-ordered
        # (from the Wikipedia page)        
        #dice.row[0] == [0, 0, 4, 4, 4, 4], #  A
        #dice.row[1] == [3, 3, 3, 3, 3, 3], #  B        
        #dice.row[2] == [2, 2, 2, 2, 6, 6], #  C
        #dice.row[3] == [1, 1, 1, 5, 5, 5], #  D

        # Miwin's dice (3 dice)
        # Miwin's Dice were invented in 1975 by the physicist Michael Winkelmann.
        # (from the Wikipedia page)
        #dice.row[0] == [1, 2, 5, 6, 7, 9],  # III
        #dice.row[1] == [1, 3, 4, 5, 8, 9],  # IV
        #dice.row[2] == [2, 3, 4, 6, 7, 8],  # V

        
        )

    # calculate the number of winnings of each round
    # (0 vs 1 and 1 vs 0, 1 vs 2 and 2 vs 1, ... m-1 vs 0 and 0 vs m-1)
    for d in range(m):
        model.add( comp[d % m,0] == Sum([dice[d % m, r1] > dice[(d+1) % m, r2]
                                       for r1 in range(n) for r2 in range(n)]))
        model.add( comp[d % m,1] == Sum([dice[(d+1) % m, r1] > dice[(d) % m, r2]
                                       for r1 in range(n) for r2 in range(n)]))


    #print model

    for library in libs:
        solver = model.load(library)
        # solver.setHeuristic('DomainOverWLDegree','AntiLex')     # 8596 failures
        solver.setHeuristic('MaxDegree','Lex')     # 324 failures

        if solver.solve():
            solver.printStatistics()
            print "comp:\n", comp
            print "probabilities:\n", [(comp[i,0].get_value()/(n*n*1.0),comp[i,1].get_value()/(n*n*1.0))  for i in range(m)]
            print "gap:", gap
            print "gap_sum:", gap_sum
            print "dice:\n", dice
            print "max_val:", max_val
            print "max_win:", max_win
            num_solutions = 1
            #while solver.getNextSolution(): 
            #    num_solutions += 1
            #    print dice
            #    print comp
            #    print "max_val:", max_val                        
            #    print "max_win:", max_win            
            print 'Number of solutions: ', num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print "getFailures:", solver.getFailures()
            print "getBacktracks:", solver.getBacktracks()            
            print "getPropags:", solver.getPropags()

        else:
            print 'No solution'
        print ''


model(['Mistral'])


