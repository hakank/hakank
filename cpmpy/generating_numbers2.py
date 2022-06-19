"""
'Generating Numbers' Puzzle in cpmpy.

https://stackoverflow.com/questions/66127644/generating-numbers-puzzle
'''
'Generating Numbers' Puzzle

I have come across the following puzzle and couldn't formulate a solution in Picat:

    You will generate 5-digit numbers, where each digit is in 1..5 and 
    different from the others, with the constraint that any three adjacent 
    digits used in one number can’t be used in another number. How many 
    different numbers can be obtained according to this rule?

For example, if we generated the number 23145, the next numbers cannot 
contain 231, 314, or 145.

I got really confused on how to store these "forbidden" sublists and how to 
check each number against them as I build the list of numbers.
'''

In my StackOverflow answer, there are a lot of errors and misguided 
approaches.

A thought about this:
    * There are 60 possible triplets and each number contains
        3 triplets: 60 / 3 = 20!

    * So my conjecture is that the maximum length is 20.
        Let's search for such a sequence. 

    * And one should rather talk about a set of numbers since the
        order is of no importance.

This model finds a solution is 0.06s (1s runtime). Here's the first solution:

CpSolverResponse summary:
status: OPTIMAL
objective: NA
best_bound: NA
booleans: 120
conflicts: 13
branches: 59
propagations: 197
integer_propagations: 256
restarts: 0
lp_iterations: 665
walltime: 0.0634056
usertime: 0.0634056
deterministic_time: 0.00482642
primal_integral: 0

ps: [3, 15, 16, 19, 23, 28, 32, 36, 44, 59, 63, 64, 67, 71, 72, 80, 84, 98, 102, 110]
[1, 2, 4, 5, 3]
[1, 4, 3, 5, 2]
[1, 4, 5, 2, 3]
[1, 5, 2, 4, 3]
[1, 5, 4, 3, 2]
[2, 1, 5, 3, 4]
[2, 3, 4, 1, 5]
[2, 4, 1, 3, 5]
[2, 5, 3, 1, 4]
[3, 2, 5, 4, 1]
[3, 4, 2, 5, 1]
[3, 4, 5, 1, 2]
[3, 5, 1, 4, 2]
[3, 5, 4, 2, 1]
[4, 1, 2, 3, 5]
[4, 2, 3, 1, 5]
[4, 3, 1, 2, 5]
[5, 1, 3, 2, 4]
[5, 2, 1, 3, 4]
[5, 3, 2, 1, 4]

Numbers: ['12453', '14352', '14523', '15243', '15432', '21534', '23415', '24135', '25314', '32541', '34251', '34512', '35142', '35421', '41235', '42315', '43125', '51324', '52134', '53214']
status: ExitStatus.OPTIMAL (0.063405612 seconds)

python3 generating_numbers2.py 20  1,90s user 1,46s system 322% cpu 1,043 total


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from itertools import permutations


def triplets(p):
    """
    Generate the three triplets for permutation p
    """
    return [p[i:i+3] for i in range(3)]

def check_perm(tri1,tri2):
    """
    Check that these permutations (representing by their triplets) 
    are compatible, i.e. don't contain a common triplet.
    """
    for t1 in tri1:
        for t2 in tri2:
            if t1 == t2:
                return False
    return True
    

def generating_numbers(m=20):

    model = Model()

    print("m:", m)

    n = 5

    # Generate the 120 permutations of 1..5
    perms = [[*p] for p in (permutations(range(1,n+1)))]
    p_len = len(perms) # Well, we know it's 120.

    # Generate a matrix of compatible permutations,
    # i.e. A[p1,p2] = 1 means that ps[p1] and ps[p2] don't
    # have any triplets in common.
    A = {}
    for p1 in range(p_len):
        A[(p1,p1)] = 1
        tri1 = triplets(perms[p1])
        for p2 in range(p1):
            tri2 = triplets(perms[p2])
            if check_perm(tri1,tri2):
                A[(p1,p2)] = 1
                A[(p2,p1)] = 1
            else:
                A[(p1,p2)] = 0
                A[(p2,p1)] = 0

    x = boolvar(shape=p_len,name="x")
    
    model += [sum(x) == m]
    # model += [x[0] == 1] # symmetry breaking
    for i in range(p_len):
        for j in range(i):
            # Should i and j be in the set?
            # (x[i] /\ x[J] ) => A[(i,j)]
            model += [(x[i] & x[j]) <= (A[i,j] )]


    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.log_search_progress = True
    # ss.ort_solver.parameters.num_search_workers = 8
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve=False
    # ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if ss.solve():
        ps = [i  for i in range(p_len) if x[i].value()==1]
        assert len(ps) == m, f"Length of ps ({len(ps)} is not m ({m}))"

        print("ps:", ps)
        for p in ps:
            print(perms[p])
        print()
        print("Numbers:",[int("".join([str(pp) for pp in perms[p]])) for p in ps])
        print("status:",ss.status())

    print()
 
m = 20
if len(sys.argv) > 1:
    m = int(sys.argv[1])
generating_numbers(m)

# for n in range(1,20+1):
#    generating_numbers(n)
    
