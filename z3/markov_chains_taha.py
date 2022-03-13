#  
# Markov Chains in z3.
#
# From Hamdy Taha "Operations Research" (8th edition), page 649ff.
#  Fertilizer example.

#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3_utils_hakank import *

#
# calculate the mean first return time from a steady state probability array
#
def get_mean_first_return_time(prob, mfrt):
    return [mfrt[i] == 1.0/prob[i] for i in range(len(prob))]

#
# Calculates the steady state probablity of a transition matrix m
#
def steady_state_prob(m, prob):
    n = len(m)
    constraints = []
    for i in range(n):
        constraints += [prob[i] == Sum([prob[j]* m[j][i] for j in range(n)])]
    
    constraints += [Sum(prob) == 1.0]

    return constraints
            


def markov_chains(mat,cost):

    s = SimpleSolver()

    n = len(mat)

    # Probabilities
    p = [Real(f"p[{i}]") for i in range(n)]
    for i in range(n):
        s.add(p[i] >= 0, p[i] <= 1)
    s.add(Sum(p) == 1)

    
    mean_first_return_time = [Real(f"mean_first_return_time[{i}]") for i in range(n)]
    tot_cost = Real("tot_cost")

    s.add(steady_state_prob(mat, p))
    s.add(tot_cost == Sum([cost[i]*p[i] for i in range(n)]))
    s.add(get_mean_first_return_time(p, mean_first_return_time))


    while s.check() == sat:
        mod = s.model()
        print("tot_cost:", float(mod[tot_cost].as_decimal(6).replace("?","")))
        print("p:", [float(mod[p[i]].as_decimal(6).replace("?","")) for i in range(n)])
        print("mean_first_return_time:", [float(mod[mean_first_return_time[i]].as_decimal(6).replace("?","")) for i in range(n)])        
        print()
        getDifferentSolution(s,mod,p,mean_first_return_time,[tot_cost])


# page 651
cost = [100.0, 125.0, 160.0]


mat1 = [[0.3, 0.6,  0.1],
       [0.1, 0.6,  0.3],
       [0.05, 0.4, 0.55]]

# the transition matrix page 650
mat2 = [[0.35, 0.6,  0.05],
        [0.3, 0.6,  0.1],
        [0.25, 0.4, 0.35]]


print("mat1:")
markov_chains(mat1,cost)

print("\nmat2:")
markov_chains(mat2,cost)

