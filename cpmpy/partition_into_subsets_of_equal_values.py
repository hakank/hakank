"""
Partition into subset of equal sums in cpmpy.

From Programmers Stack Exchange (C#)
http://programmers.stackexchange.com/questions/153184/partitioning-set-into-subsets-with-respect-to-equality-of-sum-among-subsets
Partitioning set into subsets with respect to equality of sum among subsets
'''
let say i have {3, 1, 1, 2, 2, 1,5,2,7} set of numbers, I need to split the 
numbers such that sum of subset1 should be equal to sum of subset2 
{3,2,7} {1,1,2,1,5,2}. First we should identify whether we can split number(one 
way might be dividable by 2 without any remainder) and if we can, we should 
write our algorithm two create s1 and s2 out of s.

How to proceed with this approach? I read partition problem in wiki and even in some 
articles but i am not able to get anything. Can someone help me to find the 
right algorithm and its explanation in simple English?
'''

In my solution to the question I show some possible solutions in MiniZinc and
Google or-tools/C#:
http://programmers.stackexchange.com/a/153215/13955


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math,string
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def random_s(max_val,n,num_subsets):
    s = np.random.randint(1,max_val+1,n)
    while sum(s) % num_subsets != 0:
        s[np.random.randint(0,n)] += 1

    return cpm_array(s)
  

def partition_into_subsets_of_equal_values(s,num_subsets,num_sols=0):

    n = len(s)
    if n < 20:
        print(s)

    partition_sum = sum(s) // num_subsets
    print("n:",n,"num_subsets:",num_subsets,"partition_sum:",partition_sum)

    # variables
    x = intvar(0,num_subsets-1,shape=n,name="x")

    # constraints
    model = Model([x[0] == 0 # symmetry breaking
                   ])

    for k in range(num_subsets):
        model += [sum([s[i]*(x[i]==k) for i in range(n)]) == partition_sum]

    def print_sol():
        xs = x.value()
        print(xs)
        for i in range(num_subsets):
            print("subset:",i,end=" ")
            p = []
            for j in range(n):
                if xs[j] == i:
                    p.append(s[j])
            print("p:",p,"sum:", sum(p))
        print()

    ss = CPM_ortools(model)    
    # ss.ort_solver.parameters.max_time_in_seconds = timeout
    # ss.ort_solver.parameters.num_search_workers = num_procs 
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print("num_solutions:",num_solutions)
    if num_sols == 1:
        print(ss.status())
        print("Num conflicts:", ss.ort_solver.NumConflicts())
        print("NumBranches:", ss.ort_solver.NumBranches())
        print("WallTime:", ss.ort_solver.WallTime())
    
    print()

instances = {
    # The problem cited above
    "p1": {
    "s" : [3, 1, 1, 2, 2, 1, 5, 2, 7],
    "num_subsets" : 2
    },
    # with 3 subsets instead
    "p2": {
    "s" : [3, 1, 1, 2, 2, 1, 5, 2, 7],
    "num_subsets" : 3
    },
}

for p in instances:
  print(f"problem: {p}")
  s = instances[p]["s"]
  num_subsets = instances[p]["num_subsets"]  
  partition_into_subsets_of_equal_values(s,num_subsets)

# A larger random instance
num_subsets = 3
max_val = 100
n = 1000 # 10000
gen_s = random_s(max_val,n,num_subsets)
num_sols = 1
partition_into_subsets_of_equal_values(gen_s,num_subsets,num_sols)
