# 
# Magic squares in Z3
#
# Times for n=2..10:
# 
# n: 2 : 0.008773565292358398
# n: 3 : 0.006280183792114258
# n: 4 : 0.07515954971313477
# n: 5 : 0.1314847469329834
# n: 6 : 0.47011852264404297
# n: 7 : 0.735698938369751
# n: 8 : 1.5421934127807617
# n: 9 : 5.283749341964722
# n: 10 : 44.89334321022034
#
#  
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *
import time

def print_square(mod,x,n):
    for i in range(n):
        for j in range(n):
            print("%2s" % mod.eval(x[(i, j)]),end=" ")
        print()
    

def magic_square(n,all=0):
    start = time.time()
    
    sol = SolverFor("QF_FD")
    
    x = {}
    for i in range(n):
        for j in range(n):
            x[(i, j)] = Int("x(%i,%i)" % (i, j))
            sol.add(x[(i,j)] >= 1,  x[(i,j)] <= n*n)
        
    s = Int("s")
    sol.add(s >= 1, s <= n*n*n) # Strange that it's faster with this
    sol.add(s == n*(n*n+1)//2 ) # since we set the value here
    # s = n*(n*n+1)//2 # It's slower with a fixed value of s!
    sol.add(Distinct([x[(i, j)] for i in range(n) for j in range(n)])) # flattened
    [sol.add(Sum([x[(i, j)] for j in range(n)]) == s) for i in range(n)]
    [sol.add(Sum([x[(i, j)] for i in range(n)]) == s) for j in range(n)]

    sol.add(Sum([x[(i, i)] for i in range(n)]) == s)  # diag 1
    sol.add(Sum([x[(i, n - i - 1)] for i in range(n)]) == s)  # diag 2

    # symmetry breaking (slower and might give no solution, e.g. for n=3)
    # sol.add(x[(0,0)] == 1)

    print(sol.check())
    end = time.time()
    value = end - start
    print("Time to sat: ", value)
    if sol.check() == sat:
        mod = sol.model()
        print("s:", mod.eval(s))
        # print("s:", s)
        print_square(mod, x,n)
    else:
        print("No solution found!")

    end = time.time()
    value = end - start
    print("Time all: ", value)
    return value

times = {}
for n in range(2,10+1):
    print("Testing ", n)
    time_value = magic_square(n)
    times[n] = time_value
    print()

# Print the summary
for n in times:
    print("n:",n, ":", times[n])
