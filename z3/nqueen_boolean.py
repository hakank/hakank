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
# Tactic: simplify + lia
# N        Time (s)
# -----------------
#    4     0.010567665100097656
#    8     0.012500524520874023
#   10     0.019457578659057617
#   12     0.02635049819946289
#   20     0.06975150108337402
#   50     0.42316365242004395
#  100     1.858893632888794
#  200     8.13275671005249
#  500    70.68672609329224
# 1000   393.57833433151245
#
# Tactic: simplify + smt (slower than simplify + lia)
# N: 4 Time:  0.010270357131958008 Num solutions: 1
# N: 8 Time:  0.012297630310058594 Num solutions: 1
# N: 10 Time:  0.01877903938293457 Num solutions: 1
# N: 12 Time:  0.025954484939575195 Num solutions: 1
# N: 20 Time:  0.06907916069030762 Num solutions: 1
# N: 50 Time:  0.4211549758911133 Num solutions: 1
# N: 100 Time:  1.7422709465026855 Num solutions: 1
# N: 200 Time:  7.764967203140259 Num solutions: 1
# N: 500 Time:  69.89579677581787 Num solutions: 1
# N: 1000 Time:  413.6545977592468 Num solutions: 1

# Tactic: simplify, qffd (slightly slower than simplify + lia)
# N: 4 Time:  0.011030912399291992 Num solutions: 1
# N: 8 Time:  0.013289451599121094 Num solutions: 1
# N: 10 Time:  0.020516633987426758 Num solutions: 1
# N: 12 Time:  0.027957677841186523 Num solutions: 1
# N: 20 Time:  0.0716705322265625 Num solutions: 1
# N: 50 Time:  0.43743395805358887 Num solutions: 1
# N: 100 Time:  1.8893873691558838 Num solutions: 1
# N: 200 Time:  8.345442295074463 Num solutions: 1
# N: 500 Time:  70.81256651878357 Num solutions: 1
# N: 1000 Time:  396.6864056587219 Num solutions: 1
#
#
# Tactic simplify + lia seems to be the faster solver.
# 

#
# Time to get all solutions:
#
# LIA:
#
# N: 2 Time:  0.007616519927978516 Num solutions: 0
# N: 3 Time:  0.00293731689453125 Num solutions: 0
# N: 4 Time:  0.007451534271240234 Num solutions: 2
# N: 5 Time:  0.026066303253173828 Num solutions: 10
# N: 6 Time:  0.01982903480529785 Num solutions: 4
# N: 7 Time:  0.16121602058410645 Num solutions: 40
# N: 8 Time:  0.4802219867706299 Num solutions: 92
# N: 9 Time:  2.354806423187256 Num solutions: 352
# N: 10 Time:  6.854608774185181 Num solutions: 724
# N: 11 Time:  57.4482421875 Num solutions: 2680
#
# QF_FD:
# N: 2 Time:  0.0071849822998046875 Num solutions: 0
# N: 3 Time:  0.002245187759399414 Num solutions: 0
# N: 4 Time:  0.006524324417114258 Num solutions: 2
# N: 5 Time:  0.0251462459564209 Num solutions: 10
# N: 6 Time:  0.01831984519958496 Num solutions: 4
# N: 7 Time:  0.15897750854492188 Num solutions: 40
# N: 8 Time:  0.46338963508605957 Num solutions: 92
# N: 9 Time:  2.1656906604766846 Num solutions: 352
# N: 10 Time:  5.472612380981445 Num solutions: 724
# N: 11 Time:  24.898768663406372 Num solutions: 2680
#
# Tactic: simplify + lia is much slower to get all solutions:
# N: 2 Time:  0.007582187652587891 Num solutions: 0
# N: 3 Time:  0.0028045177459716797 Num solutions: 0
# N: 4 Time:  0.00864720344543457 Num solutions: 2
# N: 5 Time:  0.0396730899810791 Num solutions: 10
# N: 6 Time:  0.02611541748046875 Num solutions: 4
# N: 7 Time:  0.33647751808166504 Num solutions: 40
# N: 8 Time:  1.759749174118042 Num solutions: 92
# N: 9 Time:  34.08975648880005 Num solutions: 352
# N: 10 Time:  359.516074180603 Num solutions: 724
# N: 11 Time: too long
# 
# Fastest solver to get all solution is QF_FD.
#

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
    # sol = SimpleSolver()
    # sol = SolverFor("QF_LIA")
    # sol = SolverFor("QF_FD") # Fastest to get all solutions
    # sol = SolverFor("LIA")

    # Fastest for getting first solution.
    t1 = Tactic('simplify')
    t2 = Tactic('lia')
    sol  = Then(t1, t2).solver()

    

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

# 8-queens: Check that it has exactly 92 solutions.
# queens(8,0,True)

# # Time to get/count all solutions
# for n in range(2,12):
#     queens(n,0,False)

