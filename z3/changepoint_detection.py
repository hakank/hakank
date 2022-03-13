#
# Changepoint detection in z3
#
# This is a simple model of changepoint detection in a time serie.
# It is a standard example in Probabilistic programming.
# Note that this model simply use mean differences for calculating the changepoint.
#
# 
# Coal miner:
# https://pymc-devs.github.io/pymc/tutorial.html
# """
# Consider the following dataset, which is a time series of recorded
# coal mining disasters in the UK from 1851 to 1962 
# [R.G. Jarrett. A note on the intervals between coal mining disasters. Biometrika, 66:191–193, 1979.]
# """
# The PyMC3 model:
# """
#             mean     sd  hdi_3#  hdi_97#  mcse_mean  mcse_sd  ess_mean  ess_sd  ess_bulk  ess_tail  r_hat
# tau       39.928  2.471  35.000   44.000      0.033    0.023    5655.0  5638.0    5686.0    7347.0    1.0
# lambda_1   3.102  0.290   2.568    3.649      0.004    0.003    5971.0  5967.0    5980.0    6367.0    1.0
# lambda_2   0.940  0.119   0.726    1.170      0.001    0.001    6684.0  6684.0    6684.0    7582.0    1.0
# """


# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def changepoint_detection(data,use_real=True):
    
    # s = SimpleSolver()
    # s = Solver()
    # s = SolverFor("QF_NRA")
    # s = SolverFor("QF_AUFLIA")
    # s = SolverFor("QF_FD")
    # s = SolverFor("NRA")
    # s = SolverFor("QF_LRA")

    if use_real:
        s = SolverFor("QF_LRA")
    else:
        # s = SolverFor("QF_FD")
        s = SolverFor("QF_LIA")

    # s = Optimize()    

    n = len(data)
    max_val = 10000000

    # decision variables
    cp = Int("cp")
    s.add(cp >= 1, cp < n-1)

    if use_real:
        mean1, mean2, mean_diff1, mean_diff2 = Reals("mean1 mean2 mean_diff1 mean_diff2")
        z = Real("z")
    else:
        mean1, mean2, mean_diff1, mean_diff2 = Ints("mean1 mean2 mean_diff1 mean_diff2")
        z = Int("z")
        
    s.add(z >= 0,mean1 >= 0, mean2>=0, mean_diff1>=0, mean_diff2>=0)
    s.add(z <= max_val,mean1 <= max_val, mean2 <= max_val, mean_diff1 <= max_val, mean_diff2 <= max_val)
    
    # s.add(mean1 == Sum([If(i < cp, data[i],0) for i in range(n)]) / cp )
    s.add(mean1*cp == Sum([If(i < cp,data[i],0) for i in range(n)]) )
    
    # s.add(mean2 == Sum([If(i >= cp, data[i],0) for i in range(n)]) / (n-cp) )
    s.add(mean2*(n-cp) == Sum([If(i >= cp,data[i],0) for i in range(n)]) )    
    
    s.add(mean_diff1 == Sum([If(i<cp, Abs(data[i]-mean1),0) for i in range(n)]))
    s.add(mean_diff2 == Sum([If(i>=cp, Abs(data[i]-mean2),0) for i in range(n)]))    

    s.add(z == mean_diff1 + mean_diff2)
    # s.add(z == Abs(mean_diff1 - mean_diff2))

    # s.minimize(z)
    
    # print(s)
    
    num_solutions = 0
    while s.check() == sat:
        mod = s.model()
        print("cp:", mod[cp])
        if use_real:
            print("z:", mod[z].as_decimal(6))
            print("mean1:", mod[mean1].as_decimal(6), "mean2:",mod[mean2].as_decimal(6))
            print("mean_diff1:", mod[mean_diff1].as_decimal(6), "mean_diff2:",mod[mean_diff2].as_decimal(6))
        else:
            print("z:", mod[z])
            print("mean1:", mod[mean1], "mean2:",mod[mean2])
            print("mean_diff1:", mod[mean_diff2], "mean_diff2:",mod[mean_diff2])
            
        print()
        s.add(z < mod[z])


##
## Coal miner
## https://pymc-devs.github.io/pymc/tutorial.html
## Gecode:
## cp:37 mean1:3.16216216216217 mean2:0.986666666666667
## z:104.2266666666666 mean_diff1:47.0 mean_diff2:57.2266666666666
##
coal_miner = [4, 5, 4, 0, 1, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1, 
              4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3, 
              0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0, 
              0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2, 
              0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1]


##
## Text message
## from "Probabilistic Programming and Bayesian Methods for Hackers":
## http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC3.ipynb
## """
## You are given a series of daily text-message counts from a user of your system.
## The data, plotted over time, appears in the chart below. You are curious to
## know if the user's text-messaging habits have changed over time, either
## gradually or suddenly. How can you model this? (This is in fact my own
## text-message data. Judge my popularity as you wish.)
## """
#
# Gecode (float model)
# cp:46 mean1:17.3695652173914 mean2:22.8275862068966
# z:711.119190404798 mean_diff1:363.326086956522 mean_diff2:347.793103448276
#
text_message = [13, 24, 8, 24, 7, 35, 14, 11, 15, 11, 22, 22, 11, 57, 11, 19, 29, 6,
                19, 12, 22, 12, 18, 72, 32, 9, 7, 13, 19, 23, 27, 20, 6, 17, 13, 10,
                14, 6, 16, 15, 7, 2, 15, 15, 19, 70, 49, 7, 53, 22, 21, 31, 19, 11,
                18, 20, 12, 35, 17, 23, 17, 4, 2, 31, 30, 13, 27, 0, 39, 37, 5, 14,
                13, 22]


# Some random instances:
#
# Generated by R:
# > c(rpois(40,3), rpois(60,12))
# 40 instances with value 3, and 60 values with values 3,
# I.e. the change point should be at point 41:
#
# Gecode (float):
# cp:41 mean1:3.09756097560976 mean2:12.25
# z:246.4756097560976 mean_diff1:63.9756097560976 mean_diff2:182.5
#
random_100 = [1, 4, 3, 1, 4, 2, 2, 7, 2, 2, 3, 5, 2, 2, 3, 1, 2, 3, 1, 1, 2, 6, 5,
              1, 2, 5, 1, 8, 4, 5, 4, 1, 6, 4, 5, 8, 2, 3, 3, 1, 
              14, 5, 13, 12, 12, 9, 10, 19, 11, 8, 9, 7, 16, 14, 15, 18, 13, 12,
              10, 11, 13, 8, 13, 10, 14, 18, 10, 15, 12, 15, 6, 15, 6, 10, 16, 13,
              13, 9, 9, 17, 13, 7, 11, 20, 7, 15, 5, 16, 15, 11, 10, 8, 14, 9, 18,
              17, 13, 13, 15, 18]


# R:
# > c(rpois(100,30), rpois(200,25))
# cp should be 101
#
# Gecode (float)
# cp:104 mean1:30.048076923077 mean2:25.6091370558376
# z:1302.574531433027 mean_diff1:469.528846153845 mean_diff2:833.045685279182
#
random_300 = [32,32,26,31,34,31,30,23,38,33,28,21,36,30,35,27,39,34,39,27,28,24,37,26,28,
              25,44,22,29,30,37,33,34,28,23,36,24,22,31,34,38,24,26,49,26,30,35,34,27,33,
              25,27,31,27,27,26,26,35,33,28,28,39,28,39,27,31,33,33,29,36,26,27,33,24,35,
              35,34,25,22,26,20,37,28,27,30,35,32,30,40,29,23,24,22,21,28,28,36,41,37,38,
              23,28,30,21,26,30,18,38,20,30,22,26,36,29,20,24,35,27,22,30,33,22,25,22,27,
              22,30,25,34,22,24,25,30,37,20,30,20,18,22,27,22,29,25,27,19,28,23,20,23,34,
              20,28,25,21,28,24,33,27,32,24,22,38,26,16,35,30,20,24,28,30,26,17,23,26,24,
              31,17,24,31,24,32,34,27,25,22,26,28,31,22,25,26,25,36,29,21,17,35,13,23,23,
              30,31,16,24,22,19,27,32,28,29,33,32,24,13,18,20,16,26,23,24,15,28,30,26,25,
              21,30,22,21,23,35,20,20,25,25,29,36,36,19,25,29,24,22,29,23,33,29,25,22,21,
              29,28,21,34,29,26,19,28,27,23,33,22,22,33,17,40,18,23,27,26,24,21,23,20,24,
              27,28,17,29,23,27,26,33,28,31,22,30,17,27,22,29,28,24,21,28,28,21,26,22,26]

#
# R:
# > c(rpois(110,3), rpois(220,5))
# cp should be at 111.
#
# Gecode (float)
# cp:114 mean1:2.96491228070176 mean2:5.3410138248848
# z:510.192012288785 mean_diff1:151.666666666667 mean_diff2:358.525345622118
#
random_330 = [1, 1, 3, 3, 4, 2, 5, 2, 3, 5, 1, 0, 0, 4, 2, 2, 2, 4, 2, 3, 2, 5, 2, 3, 3,
              2, 1, 2, 1, 3, 2, 5, 1, 5, 8, 4, 5, 1, 3, 5, 3, 5, 2, 5, 7, 4, 5, 4, 2, 4,
              3, 4, 2, 4, 1, 3, 1, 4, 4, 2, 4, 1, 3, 6, 2, 1, 3, 6, 3, 2, 0, 6, 4, 3, 3,
              1, 7, 4, 6, 5, 4, 2, 5, 5, 2, 0, 1, 1, 3, 4, 3, 1, 1, 1, 3, 5, 1, 3, 1, 3,
              2, 6, 2, 3, 3, 2, 4, 1, 4, 1, 3, 4, 3,11, 8,10, 7, 6, 7, 6, 6,10, 9, 6, 4,
              3, 7,13, 7, 5, 3, 5, 5, 6, 3, 5, 5, 4, 5, 1, 7, 4, 7, 2, 6, 4, 7, 6, 4, 6,
              4, 4, 5, 4, 4, 8, 2, 4, 3, 4, 6, 4, 6, 3, 4, 4, 2, 4, 5, 5, 4, 5, 4, 5, 7,
              7, 3, 3, 7, 9, 8, 7, 4, 3, 7, 5, 5, 5, 4,10, 4, 1, 6, 4, 6, 6, 5, 8, 5, 3,
              8, 6, 6, 6, 6, 5, 6, 7, 3, 7, 7, 3, 6, 6, 1,12, 1, 5, 5, 8, 6, 8, 4, 6, 5,
              3, 5, 5, 3, 5, 9, 2, 4, 5, 5, 3, 4, 7, 5, 7, 4, 3, 6, 7, 4, 5, 8, 5, 5, 6,
              5, 6, 6, 5, 5, 1, 8, 3, 7, 4, 5, 4, 6, 5, 1, 3, 6, 4, 6, 3, 7, 6, 9, 4, 5,
              5, 4, 4, 6, 6, 3, 9, 3, 5, 4, 6, 6,12, 7, 4, 7, 5, 6, 8, 6, 7, 8, 6, 3, 3,
              7, 3,10, 5, 5, 6, 2, 5, 5, 2, 3, 6, 6, 4,11, 3, 7, 5, 7, 1, 1, 5, 8, 7, 5,
              4, 6, 9, 8, 3]


use_real = True

changepoint_detection(coal_miner,use_real)
# changepoint_detection(text_message,use_real)
# changepoint_detection(random_100,use_real)
# changepoint_detection(random_300,use_real)
# changepoint_detection(random_330,use_real)
