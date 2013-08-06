#!/usr/bin/python
"""
Diet problem in Numberjack.

Standard Operations Research example.


Minimize the cost for the products:
Type of                        Calories   Chocolate    Sugar    Fat
Food                                      (ounces)     (ounces) (ounces)
Chocolate Cake (1 slice)       400           3            2      2
Chocolate ice cream (1 scoop)  200           2            2      4
Cola (1 bottle)                150           0            4      1
Pineapple cheesecake (1 piece) 500           0            4      5


Compare with the following models:
* MiniZinc: http://www.hakank.org/minizinc/diet1.mzn
* Choco   : http://www.hakank.org/choco/Diet.java
* Comet   : http://www.hakank.org/comet/diet.co
* ECLiPSE : http://www.hakank.org/eclipse/diet.ecl
* Gecode  : http://www.hakank.org/gecode/diet.cpp
* Gecode/R: http://www.hakank.org/gecode_r/diet.rb
* JaCoP   : http://www.hakank.org/JaCoP/Diet.java
* Tailor/Essence': http://www.hakank.org/tailor/diet1.eprime

This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

# from NumberjackSolver import Solver
# from Mistral import Solver
# Here SCiP works!
# from SCIP import Solver


def diet1(libs):

    n = 4;
    price  = [ 50, 20, 30, 80]; # in cents
    limits = [500,  6, 10,  8]; # requirements for each nutrition type

    # nutritions for each product
    calories  = [400, 200, 150, 500];
    chocolate = [  3,   2,   0,   0];
    sugar     = [  2,   2,   4,   4];
    fat       = [  2,   4,   1,   5];
    
    x         = VarArray(n, 0, 10000)
    cost      = Variable(0, 1000)
    model = Model(
        Sum(x, calories)  >= limits[0],
        Sum(x, chocolate) >= limits[1],
        Sum(x, sugar)     >= limits[2],
        Sum(x, fat)       >= limits[3],
        cost == Sum(x, price),
        Minimise(cost)
        )
    
    for library in libs:
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print "cost: ", cost
            print "x: ", x
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''

# diet1(('SCIP','Mistral','NumberjackSolver'))
# diet1(('Mistral','NumberjackSolver'))
diet1(['Mistral'])
