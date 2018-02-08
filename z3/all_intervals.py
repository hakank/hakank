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
from z3_utils_hakank import *

def all_interval(n):

    # This model is slow, so I experiment a bit here
    
    # sol = Solver()
    # sol = SolverFor("LIA") # Faster
    # sol = SolverFor("LRA") # Faster than LIA
    # sol = SolverFor("NRA") # Faster than LIA
    # sol = SolverFor("NIA") # Slower
    # sol = SolverFor("BV") # Slower
    sol = SolverFor("QF_LIA") # Faster
    # sol = SolverFor("QF_NIA") # Slower
    # sol = SolverFor("AUFLIRA") # Slow
   
    # series = Array("series", IntSort(), IntSort())
    # series = Array("series", BitVecSort(8), BitVecSort(8))
    series = IntVector("series",n )
    # series = [BitVec("series%i" % i,8) for i in range(n)]
    # series = [Int("s%i" % i) for i in range(n)]
    for i in range(n):
        sol.add(series[i] >= 0, series[i] <= n-1)

    # differences = Array("differences", IntSort(), IntSort())
    # differences = Array("differences", BitVecSort(8), BitVecSort(8))
    differences = IntVector("differences",n-1)
    # differences = [BitVec("differences%i" % i,8 ) for i in range(n-1)]
    # differences = [Int("d%i" % i) for i in range(n-1)]
    for i in range(n-1):
        sol.add(differences[i] >= 1, differences[i] <= n-1)

    # C1: Each pitch class occurs exactly once
    # GCAD: Exploitation of alldifferent() global constraint
    sol.add(Distinct([series[i] for i in range(n)]))
    
    # C2: Differences between neighbouring notes are all different
    # AUX: Addition of auxiliary predicates
    # Auxiliary predicate stores the interval between pairs of neighbouring notes
    for i in range(n-1):
        sol.add(differences[i] == Abs(series[i+1] - series[i]))

    # GCAD: Exploitation of alldifferent() global constraint
    sol.add(Distinct([differences[i] for i in range(n-1)] ))
   
    # SBSO: Symmetry-breaking by selective ordering
    # The first note is less than last one
    sol.add(series[0] < series[n-1])

    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        ss = [mod.eval(series[i]) for i in range(n)]
        print "series       : ", ss
        dd = [mod.eval(differences[i]) for i in range(n-1)]
        print "differences  :   ", dd
        print "num_solutions: ", num_solutions
        getDifferentSolution(sol,mod,series,differences)
        print
    print "num_solutions:", num_solutions
    # print sol.statistics()


n=9
all_interval(n)
