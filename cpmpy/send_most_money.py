"""
SEND+MOST=MONEY in CPMpy

Show all solutions with the maximum value of MONEY.
  
Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
import numpy
from cpmpy_hakank import *

def send_most_money(MONEY=0):
    x = intvar(0,9,shape=8)
    s,e,n,d,m,o,t,y = x
    money = intvar(0,99999)

    constraints = [money == 10000*m + 1000*o + 100*n + 10*e + y,
                   (s*1000 + e*100 + n*10 + d) + (m*1000 + o*100 + s*10 + t) == money,
                   s > 0,m > 0,
                   AllDifferent(x)]

    if MONEY == 0:
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
            get_different_solution(ss,x)
            

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
            # This is the optimal value
            MONEY = money.value()
            ss += [money == MONEY]
        get_different_solution(ss,x)
    print("num_solutions:",num_solutions)
    


max_money = send_most_money(0)
print("MONEY:", max_money)
print("\nAll optimal solutions:")
send_most_money(max_money)

print("\nAnother approach:")
send_most_money2()
