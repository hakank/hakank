#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# 1RM calculations / Comparing Reps+Weights in Z3
#
# Calculation of 1RM for weight training and age adjustment of weights.
# See below for sources.
#
# Example: 
# We want to know how much a session of 6 reps of 87kg corresponds to
# a session of 12 reps. Answer: 
#
#   OneRM  : 101.03kg age adjusted: 152.96kg
#   Weight1: 87.00kg age adjusted: 131.72kg
#   Weight2: 70.16kg age adjusted: 106.22kg
#
# I.e. a session of 6 x 87kg corresponds to a session with 12 x 70.16kg,
# with an 1RM of 101.03kg: this is the value that binds the two sessions
# (weights x reps) to each other.
#
# Also we see the age adjusted weights for Weight1, Weight2, and 1RM.
# For a 60 yo man this corresponds to weight x 1.514.
#
# We can also study - since this is a CP model - how much one have to
# lift in order to have a 1RM of 100kg for 6 reps and 12 reps, respectively:
#
#   OneRM  : 100.00kg age adjusted: 151.40kg
#   Weight1: 86.11kg age adjusted: 130.37kg
#   Weight2: 69.44kg age adjusted: 105.14kg
#
#  [I've have been there for both 6 and 12 reps.
#    My best so far:
#    - 12 x 72kg -> 1RM of 103.68kg (age adjusted:  156.97kg)
#    -  4 x 92kg -> 1RM of 101.03kg (age adjusted 152.96kg)
#    -  6 x 87kg -> 1RM of 101.03kg (== 4 x 92kg)
#  ]
#
# Note: This don't give the same results as http://strengthlevel.com/ :
# - Weight*Reps don't give the same OneRM, this model has higher values.
#   Though brzycki's seems to be nearer than epley's.
# - Age adjustments are higher in this model.

#
# Sources:
#
# Calculations of 1 RM based on nRM;
# The formulas is from Maud & Foster  "Physiological Assessments of"
# " Human Fitneess" (2 nd ed), page 140.
#
# (I assume kg in the formulas, but not sure if that' s correct or if it would matter.)
#    
# Also see: https://en.wikipedia.org/wiki/One - repetition_maximum ;
# 
# The age coefficient (1.514 for age 60) is e.g from Meltzer - Faber Age coeffcients:
#    http://www.mastersweightlifting.org/forms/meltzer.htm
#
# also:
#    http://dinosaurtraining.blogspot.se/2016/12/the-difference-between-age-30-and-age-60.html
#
# Age coefficients: http://www.mastersweightlifting.org/forms/meltzer.htm 
#
# coeffs = {
#    {30, 1}, {31, 1.016}, {32, 1.031}, {33, 1.046}, {34, 1.059}, 
#    {35, 1.072}, {36, 1.083}, {37, 1.096}, {38, 1.109}, {39, 1.122}, 
#    {40, 1.135}, {41, 1.149}, {42, 1.162}, {43, 1.176}, {44, 1.189}, 
#    {45, 1.203}, {46, 1.218}, {47, 1.233}, {48, 1.248}, {49, 1.263}, 
#    {50, 1.279}, {51, 1.297}, {52, 1.316}, {53, 1.338}, {54, 1.361}, 
#    {55, 1.385}, {56, 1.411}, {57, 1.437}, {58, 1.462}, {59, 1.488}, 
#    {60, 1.514}, {61, 1.541}, {62, 1.568}, {63, 1.598}, {64, 1.629}, 
#    {65, 1.663}, {66, 1.699}, {67, 1.738}, {68, 1.779}, {69, 1.823}, 
#    {70, 1.867}, {71, 1.91}, {72, 1.953}, {73, 2.004}, {74, 2.06}, 
#    {75, 2.117}, {76, 2.181}, {77, 2.255}, {78, 2.336}, {79, 2.419}, 
#    {80, 2.540}, {81, 2.597}, {82, 2.702}, {83, 2.831}, {84, 2.981}, 
#    {85, 3.153}, {86, 3.352}, {87, 3.58}, {88, 3.843}, {89, 4.145}, 
#    {90, 4.493}};
#
# Strangely brzycki's formula yield a lower 1Rm than 10RM!
# Changed to Wikipedia's version:
# brzycki[weight_, reps_] :=  
#   weight * (36./(37 - reps));(* weight/1.0278 - 0.0278*reps*)
#
# epley[weight_, reps_] := 0.033*weight *reps + weight;
# epley2[weight_, reps_] := weight (1 + reps/30.);
# oconner[weight_, reps_] := weight*(1 + 0.025*reps);
# lombardi[weight_, reps_] := weight * reps^0.10;
# mcglothin[weight_, reps_] := 100*weight/(101.3 - 2.67123*reps);
# wathan[weight_, reps_] := 100*weight/(48.8 + 53.8 Exp[-0.075 reps]); 
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


#
# calculate the OneRM | weights | reps according to the oneRM formula.
#
# Note: age must be a fixed integer.
#
def oneRM(age,weight1In=None, reps1In=None, weight2In=None, reps2In=None, OneRMIn=None):
    
    sol = SolverFor("QF_NIA")
    # sol = Solver()

    # Age coefficients, starts at 30 years old
    age_coeffs = [
        1    , 1.016, 1.031, 1.046, 1.059, 
        1.072, 1.083, 1.096, 1.109, 1.122, 
        1.135, 1.149, 1.162, 1.176, 1.189, 
        1.203, 1.218, 1.233, 1.248, 1.263, 
        1.279, 1.297, 1.316, 1.338, 1.361, 
        1.385, 1.411, 1.437, 1.462, 1.488, 
        1.514, 1.541, 1.568, 1.598, 1.629, 
        1.663, 1.699, 1.738, 1.779, 1.823, 
        1.867, 1.910, 1.953, 2.004, 2.060, 
        2.117, 2.181, 2.255, 2.336, 2.419, 
        2.540, 2.597, 2.702, 2.831, 2.981, 
        3.153, 3.352, 3.580, 3.843, 4.145, 
        4.493 
        ]

    # variables

    age_coeff = 1.0 * age_coeffs[age - 30]

    weight1 = Real("weight1")
    if weight1In != None:
        sol.add(weight1 == weight1In)
    sol.add(weight1 >= 0, weight1 <= 500)

    weight2 = Real("weight2")
    if weight2In != None:
        sol.add(weight2 == weight2In)
    sol.add(weight2 >= 0, weight2 <= 500)        
      
    reps1 = Int("reps1")
    if reps1In != None:
        sol.add(reps1 == reps1In)
    sol.add(reps1 >= 1, reps1 <= 30)
        
    reps2 = Int("reps2")
    if reps2In != None:
        sol.add(reps2 == reps2In)
    sol.add(reps2 >= 1, reps2 <= 30)

    OneRM = Real("oneRM")
    if OneRMIn != None:
        sol.add(OneRM == OneRMIn)
    sol.add(OneRM >= 1)

    weight1Age = Real("weight1Age") # weight1 age adjusted
    weight2Age = Real("weight2Age") # weight2 age adjusted
    OneRMAge   = Real("OneRMAge")   # OneRM age adjusted


    # constraints

    # epley's formula for 1RM given weight and reps
    # sol.add(OneRM == weight1*(1 + reps1/30.0)) # Don't work in all cases
    # sol.add(OneRM == weight2*(1 + reps2/30.0))
    sol.add(reps1*weight1 == 30 * (OneRM - weight1)) # this works
    sol.add(reps2*weight2 == 30 * (OneRM - weight2)) 

    # brzycki's formula: slightly lower than epley's
    # But it don't work in all cases...
    # sol.add(OneRM == weight1 * (36/(37 - reps1))) # Original formula
    # sol.add(OneRM == weight2 * (36/(37 - reps2)))
    # sol.add(reps1 == (37*OneRM - 36*weight1)/OneRM) # Better, but don't always work
    # sol.add(reps2 == (37*OneRM - 36*weight2)/OneRM) 


    sol.add(weight1Age == weight1 * age_coeff)
    sol.add(weight2Age == weight2 * age_coeff)
    sol.add(OneRMAge == OneRM * age_coeff)

    # print sol
    print(sol.check())
    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        # print mod
        print("OneRM: %6.6skg (age adj.: %6.6skg)" % (mod.eval(OneRM).as_decimal(4), mod.eval(OneRMAge).as_decimal(4)))
        print("Weight1: %6.6skg (age adj.: %6.6skg)  Reps: %4.3s" % (mod.eval(weight1).as_decimal(4), mod.eval(weight1Age).as_decimal(4),mod.eval(reps1).as_long()))
        print("Weight2: %6.6skg (age adj.: %6.6skg)  Reps: %4.3s" % (mod.eval(weight2).as_decimal(4), mod.eval(weight2Age).as_decimal(4),mod.eval(reps2).as_long()))
        print()
        getDifferentSolution(sol,mod, [OneRM,weight1,weight2,reps1,reps2,weight1Age,weight2Age,OneRMAge])
    print()
    
    print("num_solutions:", num_solutions)
   

age = 61

weight1 = 100 # None
weight2 = None

reps1 = 6
reps2 = 12 # None

OneRM = None

oneRM(age,weight1,reps1,weight2,reps2,OneRM)
