# 
# n-queens problem in Z3
#
# This is a boolean representation.
# Using Pseudo-Boolean AtMost and AtLeast.
# 
# (Port from by Picat queens_bool.pi model)
#
# This is much faster than the Int 0..1 model
# in nqueen_boolean_int.py
#
# Time to first solution:
# 
# SolverFor("LIA"):
# N       Time (s)
# -----------------------------
#    4    0.011156558990478516
#    8    0.013314008712768555
#   10    0.02015233039855957
#   12    0.028399229049682617
#   20    0.07218146324157715
#   50    0.4656333923339844
#  100    1.9145307540893555
#  200    8.468226671218872
#  500   74.07246398925781
# 1000  455.88581705093384
#
# Solver():
# N       Time (s)
# ----------------
#    4    0.01785111427307129
#    8    0.019182682037353516
#   10    0.026563167572021484
#   12    0.03570723533630371
#   20    0.07926797866821289
#   50    0.4749271869659424
#  100    1.9440553188323975
#  200    8.955068588256836
#  500   78.1394693851471
# 1000  450.91991329193115
#
# SimpleSolver()
# N        Time (s)
# -----------------
#    4    0.010041236877441406
#    8    0.012465715408325195
#   10    0.019736051559448242
#   12    0.02806568145751953
#   20    0.06948971748352051
#   50    0.43605899810791016
#  100    1.7996938228607178
#  200    8.306203603744507
#  500   72.78762483596802
# 1000  419.86038041114807
#
# SolverFor("QF_LIA")
# N        Time (s)
# -----------------
#    4    0.01609969139099121
#    8    0.022397756576538086
#   10    0.03185749053955078
#   12    0.046097517013549805
#   20    0.12140822410583496
#   50    1.6788935661315918
#  100    9.25275182723999
#  200   71.12651658058167  (cf 8s for the other solvers)
#  500    Too long
# 
# SolverFor("QF_FD")
# N        Time (s)
# -----------------
#    4     0.010127544403076172
#    8     0.012756824493408203
#   10     0.020239830017089844
#   12     0.03033137321472168
#   20     0.15912652015686035
#   50     6.440728425979614
#  100    63.93544793128967
#  200    too long!


# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *
import time, datetime

def queens(n,num_sols=0,print_sol=False):
    
    start = time.time()
    # sol = Solver()
    sol = SimpleSolver()
    # sol = SolverFor("QF_LIA")
    # sol = SolverFor("QF_FD")
    # sol = SolverFor("LIA")

    q = [[Bool(f"q[{i},{j}") for j in range(n)] for i in range(n) ]
            
    q_flatten = [q[i][j] for i in range(n) for j in range(n)]
    # Constraints
    for i in range(n):
        sol.add(AtMost(*[q[i][j] for j in range(n)], 1))
        sol.add(AtMost(*[q[j][i] for j in range(n)], 1))
        sol.add(AtLeast(*[q[i][j] for j in range(n)], 1))
        sol.add(AtLeast(*[q[j][i] for j in range(n)], 1))
        
    for k in range(1-n-1,n):
        # sol.add(Sum([q[i][j] for i in range(n) for j in range(n) if i-j==k])  <= 1)
        t = [q[i][j] for i in range(n) for j in range(n) if i-j==k]
        if t != []:
            sol.add(AtMost(*t,1))

    for k in range(1,2*n+1):
        # sol.add(Sum([q[i][j] for i in range(n) for j in range(n) if i+j==k])  <= 1)
        t = [q[i][j] for i in range(n) for j in range(n) if i+j==k]
        if t != []:
            sol.add(AtMost(*t,1))

    # print(sol.to_smt2())
    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1        
        mod = sol.model()
        # q_val = [mod[q[i][j]] for i in range(n) for j in range(n)]
        # print("q=",q_val)
        if print_sol:
            for i in range(n):
                for j in range(n):
                    print(mod[q[i][j]], end= " ")
                print()
            print()
        if num_sols > 0 and num_solutions >= num_sols:
            break
        getDifferentSolution(sol,mod, q_flatten)
        
    # print("num_solutions:", num_solutions)

    end = time.time()
    value = end - start
    print("N:", n, "Time: ", value, "Num solutions:", num_solutions)
    # print()

num_sols = 1
print_sol = False
for n in [4,8,10,12,20,50,100,200,500,1000]:
    queens(n,num_sols,print_sol)

# Show all solutions
# queens(8,1,False)
# queens(7,0)

