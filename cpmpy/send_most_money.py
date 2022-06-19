"""
SEND+MOST=MONEY in CPMpy

Show all solutions with the maximum value of MONEY.
  
Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
import numpy
from cpmpy_hakank import *
import copy

def send_most_money(MONEY=None):
    x = intvar(0,9,shape=8)
    s,e,n,d,m,o,t,y = x
    money = intvar(0,99999)

    constraints = [money == 10000*m + 1000*o + 100*n + 10*e + y,
                   (s*1000 + e*100 + n*10 + d) + (m*1000 + o*100 + s*10 + t) == money,
                   s > 0,m > 0,
                   AllDifferent(x)]

    if MONEY == None:
        model = Model(constraints, maximize=money)
        ss = CPM_ortools(model)
        if ss.solve():
            print(x.value())
            return money.value()
    else:
        model = Model(constraints)
        ss = CPM_ortools(model)
        model += [money==MONEY]
        print(model)
        while ss.solve():
            if money.value() == MONEY:
                print(x.value(), "money:",money.value() )
            ss += any(x != x.value())
            

#
# Another approach to get all (2) optimal values
# and don't have to do two calls to the model.
#
def send_most_money2():
    
    x = intvar(0,9,shape=8)
    s,e,n,d,m,o,t,y = x
    money = intvar(0,99999)

    model = Model([money == 10000*m + 1000*o + 100*n + 10*e + y,
                   (s*1000 + e*100 + n*10 + d) + (m*1000 + o*100 + s*10 + t) == money,
                   s > 0,m > 0,
                   AllDifferent(x)
                   ])
    model.maximize(money)

    MONEY = None
    ss = CPM_ortools(model)
    num_solutions = 0
    while ss.solve() is not False:
        num_solutions += 1
        print(x.value(), "money:",money.value() )        
        if MONEY == None:
            MONEY = money.value()
            ss += [money == MONEY]
        ss += any(x != x.value())
    print("num_solutions:",num_solutions)

# Using solve and solveAll
def send_most_money3():
    
    x = intvar(0,9,shape=8)
    s,e,n,d,m,o,t,y = x
    money = intvar(0,99999)

    model = Model([money == 10000*m + 1000*o + 100*n + 10*e + y,
                   (s*1000 + e*100 + n*10 + d) + (m*1000 + o*100 + s*10 + t) == money,
                   s > 0,m > 0,
                   AllDifferent(x)
                   ])

    # Make an optimization model
    model2 = copy.copy(model)
    model2.maximize(money)

    MONEY = None
    ss = CPM_ortools(model2)
    ss.solve()
    if ss.solve():
        MONEY = money.value()

    # Find all optimal solutions
    model += money == MONEY
    ss = CPM_ortools(model)
    
    def print_sol():
        print("x:",x.value(), " money:", money.value())

    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)
    


max_money = send_most_money(None)
print("MONEY:", max_money)
print("\nAll optimal solutions:")
send_most_money(max_money)

print("\nAnother approach:")
send_most_money2()

print("\nAnother approach:")
send_most_money3()
