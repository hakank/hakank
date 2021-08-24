#=

    This is a port of the SPPL model random-sequence.pynb
    """
    In this notebook, we will implement and query the following probabilistic model of a random sequence in SPPL.

    For i=0, X[0]  is a standard normal.
    For i>0 
        if  X[i−1] is negative, then  X[i] is a standard normal
        otherwise, let  W[i] be the outcome of a fair coin:
            if  W[i]=0 , then  X[i]=0.5∗(X[i−1])2+X[i−1] 
            othewrise(sic!),  X[i] is a standard normal.

    """

    The SPPL model give the following (exact) probabilities
    (adjusted for Python's 0 base):

    * Pr(X[1]>0)		    = 0.50
    * Pr(X[1]>0 | X[2]>0)	= 0.647
    * Pr(X[1]>0 | X[3]>0)	= 0.544


    This model:

    We want the probability of X[1] > 0 given by:
    * No observation:  
           t    mean: 0.5046   std: 0.5000

    * We observe X[2] > 0
           t    mean:0.6420    std:0.4797

    * We observe X[3] > 0
           t    mean: 0.5402   std: 0.4984 

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function random_sequence() 
    X = tzeros(3)
    W = tzeros(3)
    
    X[1] ~ Normal()
    for i in 2:3
        if X[i-1] > 0
            X[i] ~ Normal(0, 1)
            W[i] ~ Dirac(0.0)
        else
            W[i] ~ Bernoulli(0.5)
            if W[i] == 0
                X[i] ~ Dirac( 0.5*X[i-1]^2 + X[i-1])  # Dirac((X[i-1]^2 / 2 + X[i-1])
            else
                X[i] ~ Normal(0, 1)
            end 
        end
    end

    # Observations:
    true ~ Dirac(X[2] > 0)
    # true ~ Dirac(X[3] > 0)

    # What is the probability that X[1] > 0?
    t ~ Dirac(X[1] > 0)
end 

model = random_sequence()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(10), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns[[:t]])
display(chns)
