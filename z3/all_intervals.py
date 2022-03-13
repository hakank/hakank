#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# All interval problem  in Z3
#
# See 
# http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=007
# """
# Problem description
# Given the twelve standard pitch-classes (c, c#, d, ...), represented by numbers 0,1,...,11, this
# problem amounts to find a series in which each pitch-class occurs exactly once and in which the
# musical intervals between neighboring notes cover the full set of intervals from the minor second
# (1 semitone) to the major seventh (11 semitones). That is, for each of the intervals, there is a
# pair of neighboring pitch-classes in the series, between which this interval appears.
#
# We consider a generalization of this problem in which the set of numbers is the range from 0 to n-1,
# for any given positive 'n'. In particular, given such 'n', the problem amounts to find a vector
#   s = (s1, ..., sn)
# that is a permutation of {0, 1,..., n-1} and such that the interval vector
#   v = (|s2 - s1|, |s3 - s2|, ..., |sn - s(n-1)|)
# is a permutation of {1, 2,..., n-1}.
#
# Problem input
# * n, the number of pitch classes 
#
# Search space
# The set of permutations of integer range [0..n-1]
#
# Constraints
# * C1: Each pitch class occurs exactly once
# * C2: Differences between neighbouring notes are all different
# """
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
import math
from z3_utils_hakank import *

#
# Generate all solutions
# 
# From https://github.com/Z3Prover/z3/issues/5765
#
# (I thought that it would be faster than the while loop,
# but it's not.)
# 
def all_smt(s, initial_terms):
    def block_term(s, m, t):
        s.add(t != m.evaluate(t, model_completion=True))
    def fix_term(s, m, t):
        s.add(t == m.evaluate(t, model_completion=True))
    def all_smt_rec(terms):
        if sat == s.check():
           m = s.model()
           yield m
           for i in range(len(terms)):
               s.push()
               block_term(s, m, terms[i])
               for j in range(i):
                   fix_term(s, m, terms[j])
               # for m1 in all_smt_rec(terms[i+1:]):
               #     yield m1
               # (From LeventErkok: more Python idiom)
               # yield from all_smt_rec(terms[i+1:])
               # And it should not be i+1, just i:
               yield from all_smt_rec(terms[i:])
               s.pop()
    # for m1 in all_smt_rec(list(initial_terms)):
    #     yield m1
    yield from all_smt_rec(list(initial_terms))


def all_interval(n):

    # sol = SimpleSolver()
    sol = SolverFor("QF_FD")
    # sol = SolverFor("QF_BV") # slower

    # a little slower than QF_FD
    # t1 = Tactic("simplify")
    # t2 = Tactic("qffd")
    # sol = Then(t1,t2).solver()
    
    bit_vec_size = math.ceil(math.log(n,2))+1
    # print("bit_vec_size:", bit_vec_size)
    series = [BitVec(f"s[{i}]",bit_vec_size) for i in range(n)]
    for i in range(n):
        sol.add(series[i] >= 0, series[i] <= n-1)

    differences = [BitVec(f"d[{i}]",bit_vec_size) for i in range(n-1)]
    for i in range(n-1):
        sol.add(differences[i] >= 1, differences[i] <= n-1)

    # C1: Each pitch class occurs exactly once
    # GCAD: Exploitation of alldifferent() global constraint
    sol.add(Distinct([series[i] for i in range(n)]))

    # GCAD: Exploitation of alldifferent() global constraint
    sol.add(Distinct([differences[i] for i in range(n-1)] ))
    
    # C2: Differences between neighbouring notes are all different
    # AUX: Addition of auxiliary predicates
    # Auxiliary predicate stores the interval between pairs of neighbouring notes
    for i in range(n-1):
        sol.add(differences[i] == Abs(series[i+1] - series[i]))
   
    # SBSO: Symmetry-breaking by selective ordering
    # The first note is less than last one
    sol.add(series[0] < series[n-1])
    sol.add(differences[0] < differences[1])    

    var = differences + series
    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        print("series       :", [mod.eval(series[i]) for i in range(n)])
        print("differences  :", [mod.eval(differences[i]) for i in range(n-1)])
        print()
        getDifferentSolution(sol,mod,var)
        # sol.add(Or([t != mod[t] for t in var]))

    # This is not faster and not slower
    # num_solutions = 0
    # for solution in all_smt(sol, var):
    #     # print(solution)
    #     num_solutions += 1
    #     print("series     :", [solution[series[i]] for i in range(n)])
    #     print("differences:", [solution[differences[i]] for i in range(n-1)])
    #     print()

    
    print("num_solutions:", num_solutions)

    # print(sol.statistics())


n=9
all_interval(n)
