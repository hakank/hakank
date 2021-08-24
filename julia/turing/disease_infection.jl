#=

    This is a port of the SPPL model disease-infection.pynb
    (The arg max value of the posterior is 330 in the SPPL model, but this is based 
     on a rather crude interval. Using a less crude, but much slower, interval give 333.)


    This model:
    parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

    num_people   671.4048   61.1521     0.6115    1.3344   2135.0113    1.0012      511.6250
       num_met   335.7222   27.9166     0.2792    0.5434   2402.8246    1.0005      575.8027

    Quantiles
    parameters       2.5%      25.0%      50.0%      75.0%      97.5% 
        Symbol    Float64    Float64    Float64    Float64    Float64 

    num_people   560.0000   628.0000   671.0000   713.0000   793.0000
       num_met   284.0000   316.0000   335.0000   354.0000   391.0250

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function disease_infection(num_infected=100) 
    num_people ~ DiscreteUniform(500,1500) # values=range(500, 1500, 20))

    # switch (num_people) cases (n in range(500, 1500, 20)):
    num_met ~ Binomial(num_people, 0.5)
    # condition ((low[n] <= num_met) < high[n])
    # switch (num_met) cases (m in range(low[n], high[n])):
    num_infected ~ Binomial(num_met, 0.3)
end 

# low  = {n : int(scipy.stats.binom.ppf(.01, n=n, p=.5)) for n in range(500, 1500)}
# high = {n : int(scipy.stats.binom.ppf(.99, n=n, p=.5)) for n in range(500, 1500)}

num_infected = 100
model = disease_infection(num_infected)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)


