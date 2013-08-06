#!/usr/bin/python
"""
Send more money in any base problem in Numberjack.

Send More Money
   SEND
 + MORE
 ------
  MONEY

using any base.

Examples:
   Base 10 has one solution:
        {9, 5, 6, 7, 1, 0, 8, 2}
   Base 11 has three soltutions:
	{10, 5, 6, 8, 1, 0, 9, 2}
	{10, 6, 7, 8, 1, 0, 9, 3}
	{10, 7, 8, 6, 1, 0, 9, 2}
      

Also, compare with the following models:
* Comet   : http://www.hakank.org/comet/send_more_money_any_base.co
* ECLiPSE : http://www.hakank.org/eclipse/send_more_money_any_base.ecl
* Essence : http://www.hakank.org/tailor/send_more_money_any_base.eprime
* Gecode  : http://www.hakank.org/gecode/send_more_money_any_base.cpp
* Gecode/R: http://www.hakank.org/gecode_r/send_more_money_any_base.rb
* MiniZinc: http://www.hakank.org/minizinc/send_more_money_any_base.mzn


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/
"""
from Numberjack import *
from Mistral import Solver
# from SCIP import Solver



def send_more_money_any_base(libs, base=10):
    print "base: ", base
    s,e,n,d,m,o,r,y = (Variable(0,base-1) for val in range(8))
    model = Model([
                              s*base**3 + e*base**2 + n*base + d +
                              m*base**3 + o*base**2 + r*base + e ==
                  m*base**4 + o*base**3 + n*base**2 + e*base + y,
                  s > 0,
                  m > 0,
                  AllDiff((s,e,n,d,m,o,r,y))
                  ]
                  )
    
    for library in libs:
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print s,e,n,d, " + ", m,o,r,e, " = ", m,o,n,e,y
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            num_solutions = 1
            while solver.getNextSolution() == SAT:
                num_solutions += 1
                print s,e,n,d, " + ", m,o,r,e, " = ", m,o,n,e,y
            print base,  ": number of solutions: ", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            
        else:
            print "No solution"
        print ''

b = 10
send_more_money_any_base(['Mistral'], b)

#for b in range(8,50):
#    send_more_money_any_base(['Mistral'], b)
    # send_more_money_any_base(['NumberjackSolver'], b)
    # send_more_money_any_base(['SCIP'], b)

