#
# 4x4 Grid equation in z3.
#
# https://puzzling.stackexchange.com/questions/102658/4x4-grid-equations?noredirect=1
# """
# Can you place all numbers from 1 to 16 into cells, such that the following 8
# equations hold? Note that the operator "/" only works for non-remainder division, i.e.
# you can have "8 / 4" but not "8 / 3". As usual multiplication and division are performed
# before addition and subtraction. Good luck!
#
#
#   A + B / C = D
#   +   -   -   +
#   E / F + G = H
#   *   *   /   /
#   I + J + K = L
#   =   =   =   =
#   M - N / O = P
#
# """

# The smt model in that is linked to in a comment
# gives a solution in 0.1s.
# Why is this z3 program so slow? The culprit is the division!
# Without division it's much faster (and QF_FD can handle it).
#
# After several experiments, I found that the fastest approach is
# QF_FD together with the ordering.
#  * Constraints
#  * Distinct
#  * Domains
# The solve time is 0.044s for first solution (system time:0,228s).
# Proving unicity takes 0.0885s (system time: 0.275s).
# 
# This ordering is for me - as a Constraint Programmer - a bit unintuitive,
# but very interesting.
# It seems that only QF_FD really benefits from this reordering!
#
import time
from z3 import *
from z3_utils_hakank import *

def solve_problem():

    # For the "original" problem, i.e. with division. Time for first solution.
    # Distinct before the domains.
    # s = Solver() # 13.12s
    # s = SimpleSolver() # 6.614s
    # s = SolverFor("QF_NIA") # 15.257s
    # s = SolverFor("QF_LIA") # 6.34s
    # s = SolverFor("QF_LIRA") # 13.155s 
    # s = SolverFor("QF_FD") # No solution! s.check() returns "unknown", because of division?!

    # Original problem, but with Distinct before the domains.
    # s = Solver() # 13.38s
    # s = SimpleSolver() # 15.73s
    # s = SolverFor("QF_NIA") # 15.321s
    # s = SolverFor("QF_LIA") # 2.423s !
    # s = SolverFor("QF_LIRA") # 13.45s
    # s = SolverFor("QF_FD") # No solution! s.check() returns "unknown", because of division?!


    # Without division, first solution, solve time. Distinct after domains.
    # s = Solver() # 0.433s
    # s = SimpleSolver() # >1min
    # s = SolverFor("QF_NIA") # 0.602s
    # s = SolverFor("QF_LIA") # > 1min
    # s = SolverFor("QF_LIRA") # 0.427s
    # s = SolverFor("QF_FD") # 0.210s
    # s = SolverFor("NIA") # 0.426s

    # 0,740s
    # t1 = Tactic("default")
    # t2 = Tactic("qflia")
    # # # t2 = Tactic("sat") # 0.72s
    # # # t2 = Tactic("sat") # 0.72s
    # s = Then(t1,t2).solver()

    # Without division, first solution, solve time.
    # Order: Distinct, Domains, Constraints, 
    # s = Solver() # 0.432s
    # s = SolverFor("QF_NIA") # 0.597s
    # s = SolverFor("QF_LIRA") # 0.431s
    # s = SolverFor("QF_FD") # 0.198s
    # s = SolverFor("NIA") # 0.424s


    # Without division, first solution, solve time.
    # Order: Constraints, Distinct, Domains, 
    # s = Solver() # 0.553s
    # s = SolverFor("QF_NIA") # 0.633s
    # s = SolverFor("QF_LIRA") # 0.548s
    s = SolverFor("QF_FD") # 0.044s!! Unicity in: 0.0885s
    # s = SolverFor("NIA") # 0.557s


    A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P = Ints("A B C D E F G H I J K L M N O P")
    All = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]


    # It's not faster if I add parenthesis...
    # s.add(A + (B / C) == D,
    #       (E / F) + G == H,
    #       I + J + K == L,
    #       M - (N / O) == P,
    #       A + (E * I) == M,
    #       B - (F * J) == N,
    #       C - (G / K) == O,
    #       D + (H / L) == P
    #     )

    # Translation of the smt code (op.cit)
    # (assert (= b (* c (- d a))))
    # (assert (= e (* f (- h g))))
    # (assert (= l (+ i (+ j k))))
    # (assert (= n (* o (- m p))))
    # (assert (= m (+ a (* e i))))
    # (assert (= n (- b (* f j))))
    # (assert (= g (* k (- c o))))
    # (assert (= h (* l (- p d))))
    s.add(
        B == C * (D - A),
        E == F * (H - G),
        L == I + (J + K),
        N == O * (M - P),
        M == A + (E * I),
        N == B - (F * J),
        G == K * (C - O),
        H == L * (P - D)        
        )
    
    s.add(Distinct(All))

    
    for i in range(len(All)):
        s.add(All[i] >= 1, All[i] <= 16)


    # print(s.to_smt2())

    if s.check() == sat:
        mod = s.model()
        print(f"{mod[A]} + {mod[B]} / {mod[C]} = {mod[D]}")
        print("+   -   -   +")
        print(f"{mod[E]} / {mod[F]} + {mod[G]} = {mod[H]}")
        print("*   *   /   /")
        print(f"{mod[I]} + {mod[J]} + {mod[K]} = {mod[L]}")
        print("=   =   =   =")
        print(f"{mod[M]} - {mod[N]} / {mod[O]} = {mod[P]}")
        # getDifferentSolution(s,mod,All)

    
t0 = time.time()
solve_problem()
t1 = time.time()
print("Time: ", t1-t0)
