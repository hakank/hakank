"""
Satisfiability Problem in cpmpy.

From GLPK:s example sat.mod
'''
SAT, Satisfiability Problem
 
Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def sat_problem():

    #  number of clauses
    m = 133

    # number of variables
    n = 42


    # '''
    # These data correspond to the instance hole6 (pigeon hole problem for
    # 6 holes) from SATLIB, the Satisfiability Library, which is part of
    # the collection at the Forschungsinstitut fuer anwendungsorientierte
    # Wissensverarbeitung in Ulm Germany
    #
    # The optimal solution is 1 (one clause cannot be satisfied)
    # '''
    # Note: This is 1-based. Adjusted to 0-based below.
    C = [[-1,-7],
         [-1,-13],
         [-1,-19],
         [-1,-25],
         [-1,-31],
         [-1,-37],
         [-7,-13],
         [-7,-19],
         [-7,-25],
         [-7,-31],
         [-7,-37],
         [-13,-19],
         [-13,-25],
         [-13,-31],
         [-13,-37],
         [-19,-25],
         [-19,-31],
         [-19,-37],
         [-25,-31],
         [-25,-37],
         [-31,-37],
         [-2,-8],
         [-2,-14],
         [-2,-20],
         [-2,-26],
         [-2,-32],
         [-2,-38],
         [-8,-14],
         [-8,-20],
         [-8,-26],
         [-8,-32],
         [-8,-38],
         [-14,-20],
         [-14,-26],
         [-14,-32],
         [-14,-38],
         [-20,-26],
         [-20,-32],
         [-20,-38],
         [-26,-32],
         [-26,-38],
         [-32,-38],
         [-3,-9],
         [-3,-15],
         [-3,-21],
         [-3,-27],
         [-3,-33],
         [-3,-39],
         [-9,-15],
         [-9,-21],
         [-9,-27],
         [-9,-33],
         [-9,-39],
         [-15,-21],
         [-15,-27],
         [-15,-33],
         [-15,-39],
         [-21,-27],
         [-21,-33],
         [-21,-39],
         [-27,-33],
         [-27,-39],
         [-33,-39],
         [-4,-10],
         [-4,-16],
         [-4,-22],
         [-4,-28],
         [-4,-34],
         [-4,-40],
         [-10,-16],
         [-10,-22],
         [-10,-28],
         [-10,-34],
         [-10,-40],
         [-16,-22],
         [-16,-28],
         [-16,-34],
         [-16,-40],
         [-22,-28],
         [-22,-34],
         [-22,-40],
         [-28,-34],
         [-28,-40],
         [-34,-40],
         [-5,-11],
         [-5,-17],
         [-5,-23],
         [-5,-29],
         [-5,-35],
         [-5,-41],
         [-11,-17],
         [-11,-23],
         [-11,-29],
         [-11,-35],
         [-11,-41],
         [-17,-23],
         [-17,-29],
         [-17,-35],
         [-17,-41],
         [-23,-29],
         [-23,-35],
         [-23,-41],
         [-29,-35],
         [-29,-41],
         [-35,-41],
         [-6,-12],
         [-6,-18],
         [-6,-24],
         [-6,-30],
         [-6,-36],
         [-6,-42],
         [-12,-18],
         [-12,-24],
         [-12,-30],
         [-12,-36],
         [-12,-42],
         [-18,-24],
         [-18,-30],
         [-18,-36],
         [-18,-42],
         [-24,-30],
         [-24,-36],
         [-24,-42],
         [-30,-36],
         [-30,-42],
         [-36,-42],
         [6,5,4,3,2,1],
         [12,11,10,9,8,7],
         [18,17,16,15,14,13],
         [24,23,22,21,20,19],
         [30,29,28,27,26,25],
         [36,35,34,33,32,31],
         [42,41,40,39,38,37]]

    # '''
    # main variables
    # 
    # To solve the satisfiability problem means to determine all variables
    # x[j] such that conjunction of all clauses C[1] and ... and C[m] takes
    # on the value true, i.e. all clauses are satisfied.
    #
    # Let the clause C[i] be (t or t' or ... or t''), where t, t', ..., t''
    # are either variables or their negations. The condition of satisfying
    # C[i] can be most naturally written as:
    #
    #    t + t' + ... + t'' >= 1,                                       (1)
    #
    # where t, t', t'' have to be replaced by either x[j] or (1 - x[j]).
    # The formulation (1) leads to the mip problem with no objective, i.e.
    # to a feasibility problem.
    #
    # Another, more practical way is to write the condition for C[i] as:
    #
    #    t + t' + ... + t'' + y[i] >= 1,                                (2)
    #
    # where y[i] is an auxiliary binary variable, and minimize the sum of
    # y[i]. If the sum is zero, all y[i] are also zero, and therefore all
    # clauses are satisfied. If the sum is minimal but non-zero, its value
    # shows the number of clauses which cannot be satisfied.
    # '''

    x = boolvar(shape=n,name="x")

    # auxiliary variables
    y = boolvar(shape=m,name="y")

    # number of unsatisfied clasues, objective to minimize
    unsat = intvar(0,m,name="unsat")


    model = Model(unsat == sum(y),x[0]==0)

    for i in range(m):
        # Adjust to 0-based by "abs(j)-1"
        model += ((sum([x[abs(j)-1] for j in C[i] if j > 0]) +
                   sum([(1 - x[abs(j)-1]) for j in C[i] if j <= 0] )  + 
                   y[i]
            ) >= 1)

    model.minimize(unsat)

    ss = CPM_ortools(model)
    num_solutions = 0
    if ss.solve() is not False:
        num_solutions += 1
        print("x:",x.value())
        print("x vars:",[i for i in range(n) if x[i].value()])
        print("y:",y.value())
        print("y unsat:",[i for i in range(m) if y[i].value()])
        print("unsat:",unsat.value())

    print("num_solutions:",num_solutions)


sat_problem()
