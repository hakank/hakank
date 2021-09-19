"""
Dividing into roughly equal sized groups in cpmpy.

From or-exchange:
'dividing into roughly equal sized groups, with a sorted list'
http://www.or-exchange.com/questions/4398/dividing-into-roughly-equal-sized-groups-with-a-sorted-list
'''
I have a problem, and it seems like it should be something that someone 
has studied before. I have a sorted list of N elements, and I want to divide 
them into K groups, by choosing K-1 split points between them. There may be 
elements with the same value, and we want to have items with same value in 
the same group. Find K groups as close in size to round(N/K) as possible.

For example, divide these 32 elements in to 4 groups of size 8:

1 1 1 1 2 2 3 3 3 3 3 3 3 3 4 4 4 4 5 5 5 5 5 6 6 6 6 7 8 9 10 10

One solution would be these 3 break points:

1 1 1 1 2 2 | 3 3 3 3 3 3 3 3 | 4 4 4 4 5 5 5 5 5 | 6 6 6 6 7 8 9 10 10
[            6                 14                 23                    ]
[    6 elts      8 elts               9 elts              9 elts          ]
  
1 1 1 1 2 2         = 6 elements,  error = abs(8-6)=2
3 3 3 3 3 3 3 3     = 8 elements,  error = abs(8-8)=0
4 4 4 4 5 5 5 5 5     = 9 elements,  error = abs(8-9)=1
6 6 6 6 7 8 9 10 10 = 9 elements,  error = abs(8-9)=1
  
total error = 4
  
Does this look familiar to anyone? I'd like an approximation algorithm if possible.
  
Thanks, Craig Schmidt
'''

Here's the solution to the instance cited above:

  a: [1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 8, 9, 10, 10]
  s: [6 8 9 9]
  differences from gsize (8):  [-2  0  1  1]
  x: [ 6 14 23]
  z: 4
  The groups:
  Group 0: [1, 1, 1, 1, 2, 2]
  Group 1: [3, 3, 3, 3, 3, 3, 3, 3]
  Group 2: [4, 4, 4, 4, 5, 5, 5, 5, 5]
  Group 3: [6, 6, 6, 6, 7, 8, 9, 10, 10]
  
  Num conflicts: 1
  NumBranches: 15
  WallTime: 0.0034946990000000004


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def equal_sized_groups(a,k):
    # a: the array of elements
    # k: number of groups

    # print("a:", a)

    # Assert that we have enough different values to
    # create k groups
    num_unique_values = len(np.unique(a))    
    assert num_unique_values >= k, f"Number of unique values ({num_unique_values}) is not >= k ({k})"

    n = len(a)
    gsize = math.ceil(n/k) # average (ideal) size of each group

    # The valid break points
    valid_breaks = [j for j in range(1,n) if  a[j] != a[j-1]]
    # print("valid_breaks:",valid_breaks)

    # Non-valid break points
    # non_valid_breaks = [j for j in range(1,n) if a[j] == a[j-1]]

    # Number of elements in each group
    s = intvar(1,n,shape=k,name="s")
    
    # The break points 
    x = intvar(1,n,shape=k-1,name="x")
    
    # Number of errors (differences to ideal sized groups), to minimize
    z = intvar(0,n,name="z")

    model = Model([z == sum([abs(s[i]-gsize) for i in range(k)]),
                   s[0] == x[0]
                   ])

    # x can only take the valid break positions
    # But this is not faster since member_of is quite inefficient!
    # model += [member_of(valid_breaks,x[j]) for j in range(k-1)]

    # x[i] is the difference of s[i-1] and s[i], i.e. size of the (i-1)'th group.
    for i in range(1,k-1):
        model += (s[i] == x[i] - x[i-1])
    model += (s[k-1] == n-x[k-2])

    # Same values must be in the same group
    for j in range(1,n):
        # This don't work since a is not an proper typed array
        # model += ( (a[j-1] == a[j]).implies(sum([x[p] == j-1 for p in range(k-1)]) == 0))
        # So let's use <= instead of implies
        model += ( (a[j-1] == a[j])<=(sum([x[p] == j for p in range(k-1)]) == 0))

    model.minimize(z)

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if ss.solve():
        print("s:",s.value())
        print(f"differences from gsize ({gsize}): ",s.value() - gsize)
        xval = x.value()
        print("x:",xval)
        x_len = len(xval)
        print("z:",z.value())
        print("The groups:")
        print("Group 0:", a[0:xval[0]]) # first group
        for i in range(1,x_len):
            print(f"Group {i}:", a[xval[i-1]:xval[i]])
        print(f"Group {x_len}:", a[xval[x_len-1]:]) # last group
        print()
        print("Num conflicts:", ss.ort_solver.NumConflicts())
        print("NumBranches:", ss.ort_solver.NumBranches())
        print("WallTime:", ss.ort_solver.WallTime())
        print()

problems = {
    "problem1" : {
    # n = 32
    "k" : 4,
    "a" : [1,1,1,1,2,2,3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,7,8,9,10,10]
    },

    "problem2" : {
    # n = 1000
    "k" : 10,
    "a" : [1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13,13,14,14,14,14,14,14,14,14,14,14,14,14,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,17,17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,26,26,26,26,26,27,27,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,31,31,31,31,31,32,32,32,32,32,32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,34,34,35,35,35,35,35,35,35,35,35,35,35,36,36,36,36,37,37,37,37,37,37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,38,38,39,39,39,39,39,39,39,39,40,40,40,40,40,40,40,41,41,41,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,42,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,50,50,50,51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,52,53,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,55,55,55,55,55,55,55,55,55,56,56,56,56,56,57,57,57,57,57,57,57,57,57,57,57,57,57,57,58,58,58,58,58,58,59,59,59,59,59,59,59,60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,63,63,63,63,63,63,63,63,63,63,63,64,64,64,64,64,64,64,64,65,65,65,65,65,65,65,65,65,66,66,66,66,66,66,66,66,66,66,66,67,67,67,67,67,67,67,67,67,67,67,68,68,68,68,68,68,68,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,71,71,71,72,72,72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,73,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,75,75,75,75,75,75,75,75,75,75,76,76,76,76,76,76,76,76,76,76,76,77,77,77,77,77,77,77,78,78,78,78,79,79,79,79,79,79,79,79,79,79,80,80,80,80,80,80,80,80,80,81,81,81,81,81,81,81,81,81,82,82,82,82,82,82,82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,84,84,84,84,84,84,84,85,85,85,85,85,85,85,85,85,85,86,86,86,86,86,86,86,86,86,87,87,87,87,87,87,87,88,88,88,88,88,88,88,88,88,89,89,89,89,89,89,89,89,89,90,90,90,90,90,90,90,90,90,90,91,91,91,91,91,91,91,91,91,91,91,91,92,92,92,92,92,92,92,92,92,92,92,92,92,93,93,93,93,93,93,93,94,94,94,94,94,94,94,95,95,95,95,95,95,95,95,95,95,95,95,96,96,96,96,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,98,98,98,98,98,98,98,98,98,98,98,99,99,99,99,99,99,99,99,99,100,100,100,100,100,100,100,100,100,100,100,100]
    }
}

for p in problems:
    print(f"\nproblem {p}")
    a = problems[p]["a"]
    k = problems[p]["k"]    
    equal_sized_groups(a,k)
