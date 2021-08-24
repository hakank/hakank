#=

    This is a port of the SPPL model election.pynb

    This model:
    Summary Statistics
    parameters        mean        std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol     Float64    Float64    Float64   Float64     Float64   Float64       Float64 

        param    299.7774    28.9717     0.2897    0.3653   6479.7518    0.9999      866.9724
            p      0.4814     0.0318     0.0003    0.0004   6508.3665    1.0002      870.8010
        votes   1925.2743   131.9134     1.3191    1.7207   6442.0098    1.0003      861.9226
          win      0.2902     0.4539     0.0045    0.0056   6597.9243    0.9999      882.7836


=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

#=
from sppl.sym_util import binspace
n = 4000
param ~= randint(low=250, high=350)
switch (param) cases (b in range(250, 350)):
    p ~= beta(a=277, b=b)
switch (p) cases (x in binspace(0, 1, 20)):
    votes ~= binom(n=n, p=(x.left + x.right)/2)
win ~= votes > 0.5*n

=#
@model function election(n=4000) 
    param ~ DiscreteUniform(250,350)
    p ~ Beta(277,param)
    votes ~ Binomial(n,p)

    win ~ Dirac(votes > 0.5*n)

end 

n = 4000
model = election(n)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)


