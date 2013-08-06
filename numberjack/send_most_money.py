#!/usr/bin/python
"""
SEND + MOST = MONEY in Numberjack.

Alphametic problem were we maximize MONEY.
   
Problem from the lecture notes:
http://www.ict.kth.se/courses/ID2204/notes/L01.pdf

Compare with the following models:
* Comet   : http://www.hakank.org/comet/send_most_money.co
* Comet   : http://www.hakank.org/comet/send_most_money2.co
* ECLiPSE : http://www.hakank.org/eclipse/send_most_money.ecl
* MiniZinc: http://www.hakank.org/minizinc/send_most_money.mzn
* Gecode/R: http://www.hakank.org/gecode_r/send_most_money2.rb
* Tailor/Essence': http://www.hakank.org/tailor/send_most_money.eprime

This model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/
"""
from Numberjack import *
from Mistral import Solver
# from SCIP import Solver


def send_more_money_any_base(libs, MONEY=0):

    base = 10
    s,e,n,d,m,o,t,y = (Variable(0,base-1) for val in range(8))
    money = Variable(0, 100000)
    model = Model([
                              s*base**3 + e*base**2 + n*base + d +
                              m*base**3 + o*base**2 + s*base + t ==
                              money,
                  money == m*base**4 + o*base**3 + n*base**2 + e*base + y,
                  money >0,
                  s > 0,
                  m > 0,
                  AllDiff((s,e,n,d,m,o,t,y))
                  ]
                  )

    if MONEY > 0:
        model.add(money == MONEY)
    else: 
        model.add(Maximise(money))
    
    for library in libs:
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            print "money: ", money
            print s,e,n,d, " + ", m,o,s,t, " = ", m,o,n,e,y
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            num_solutions = 1
            while library == "Mistral" and solver.getNextSolution():
                num_solutions += 1
                print "money: ", money
                print s,e,n,d, " + ", m,o,s,t, " = ", m,o,n,e,y
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print "number of solutions: ", num_solutions

            return money.get_value()
        else:
            print "No solution"
        print ''


# First get the maximised MONEY, and then show all solutions for
# this value
money = send_more_money_any_base(['Mistral'], 0)
send_more_money_any_base(['Mistral'], money)


