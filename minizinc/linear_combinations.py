#
# Restoring list from pair differences in MiniZinc.
#
# See the MiniZinc model http://hakank.org/minizinc/linear_combinations.mzn
# for info in this.
#
# For more on minizinc-python, see https://minizinc-python.readthedocs.io/
# Installing the package: 
#  $ pip3 install minizinc
#
#
# This MiniZinc-Python model was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my MiniZinc page: http://www.hakank.org/minizinc/
#
#
from minizinc import Instance, Model, Solver, Status
import math, time, random


#
# generate_problem(n)
#
# * generate a list of (atmost) n random integers in the range of 1..100
#   Note: the list can be slightly smaller since we are using random numbers
#         and remove the duplicates
# * cretate the pairs differences, sorted and with duplicates removed
#
def generate_problem(n,max_val):
    ll = [random.randint(1,max_val) for _ in range(n)]
    ll = list(dict.fromkeys(ll)) # remove duplicates
    ll.sort()
    return ll

#
# diffs = pair differences, sorted and remove duplicates
#
def pair_diffs(ll):
    n = len(ll)
    diffs = [abs(ll[i]-ll[j]) for i in range(n) for j in range(i+1,n)]
    diffs = list(dict.fromkeys(diffs))
    diffs.sort()
    return diffs

#
# list difference of a list
#
def list_diff(ll):
    return [ll[i]-ll[i-1] for i in range(1,len(ll))]

#
# Shifted (normalized) list
#
def shifted(ll):
    return [ll[i]-ll[0]+1 for i in range(len(ll))]

#
# Restore the list of (distinct and sorted) pair differences.
#
def restore_list(diffs, num_diffs,n, mode="first", print_solution=False):
    
    instance = Instance(solver,model)
    instance["diffs"] = diffs
    instance["num_diffs"] = num_diffs
    instance["n"] = n
    instance["mode"] = mode    

    found_solution = False
    count = 0    
    if mode == "first":
        result = instance.solve(all_solutions=False)
        if result.status != Status.UNSATISFIABLE:
            count = len(result)
            found_solution = True
            if print_solution:
                x = result["x"]
                print(x)
                print("len:", len(x))

            print("count:", count)
    else:
        result = instance.solve(all_solutions=True)
        if result.status != Status.UNSATISFIABLE:
            count = len(result)
            found_solution = True
            if print_solution:
                for i in range(len(result)):
                    x = result[i,"x"]
                    print(x,"len:", len(x), "list_diff:", list_diff(x))
                    # An experiment:
                    # print(prod(list_diff(x)), x, "len:", len(x), "prod(x):", prod(x), "list_diff:", list_diff(x), "prod(listdiff(x)):", prod(list_diff(x)))

            print("count:", count)

    return found_solution, count

#
# Returns the list 1..n
#
def extremal_list(n):
    return [i+1 for i in range(n)]

#
# product of a list
#
def prod(ll):
    p = 1
    for i in ll:
        p *= i
    return p

#
# Benchmark: l = 1..20
#
# Gecode: 10.4s
# Chuffed: 11.0s
# Ortools: > 10min ?!?
# Picat_sat: too slow
# Picat cp: 25.8s
# Picat smt: too slow


#
# The Solvers.
#
solver = Solver.lookup("gecode")
# solver = Solver.lookup("or_tools")
# solver = Solver.lookup("chuffed")
# solver = Solver.lookup("picat_sat")
# solver = Solver.lookup("picat_cp")
# solver = Solver.lookup("picat_smt")
# solver = Solver.lookup("jacop") # nope
# solver = Solver.lookup("cbc") # Nope
# solver = Solver.lookup("Choco") # Nope

model = Model("linear_combinations.mzn")

#
# extremal problem: i.e. the origin list is 1..n
#
# ll = extremal_list(20)

# Random problem
ll = generate_problem(15,100)

print("ll:", ll)
print("ll_len:", len(ll))
print("ll_shifted:", shifted(ll))
print("list_diff(ll):", list_diff(ll))

# get the pairs list (distinct and ordered)
diffs = pair_diffs(ll)

# diffs = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,30,31,32,33,34,37,38,39]
# diffs = [4,9,11,13,15,16,20,24,29,31,33,35,44]

# Picat SAT: 53.0s
# Gecode: > 2h
# 
# diffs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 61, 62, 63, 66]

num_diffs = len(diffs)
print("diffs:", diffs)
print("diffs_len:", num_diffs)

min_len = math.floor(1+math.sqrt(1+8*num_diffs)/2) # num_difference pairs = (n*(n-1)) div 2
print("min_len:", min_len)

# mode = "first"
mode = "all" 
# mode = "all_shortest" 

print_solution = True
# print_solution = False

time_start = time.time()
found_solution = False
counts = []
print()
for n in range(min_len, (min_len*3)+1):
    print("n:", n)
    
    found_solution, count = restore_list(diffs, num_diffs,n, mode, print_solution)
    counts.append(count)
    if (mode == "first" or mode == "all_shortest") and found_solution == True:
        break

time_end = time.time()
print(f"Time: {time_end-time_start}s")
if mode == "all" or mode == "all_shortest":
    print("counts:", counts)
