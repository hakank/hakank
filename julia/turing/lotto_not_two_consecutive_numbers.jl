#=
   Lotto Problem

   From some unknown source (but I quoted it here: http://www.hakank.org/sims/simulering.html)
   """
   One day I was asked 

   What is the probability that no two numbers are consecutive in
   a lottery draw?

   Information:

   National Lottery in UK is to draw 6 different numbers from 1 to 49.
   so, for example,

    (a)   1  4  7   12  19  44    - no consecutive numbers
    (b)   3  6  17  18  44  46    - 17 and 18 are consecutive 
    (c)   1  2  3   17  29  49    - 1, 2 and 3 are consecutive

   We are asking the probability that class (a) occurs. Hope this is clear.

   Observation shows that it is NOT a small number (actually near half-and-half).
   ......

   A Monte Carlo simulation experiment produced 50,558 sets of six numbers 
   between 1 and 49 with none consecutive out of 100,000 trials giving an 
   estimated probability of .50558.
   """

   Summary Statistics
   parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

        prob    0.5054    0.5000     0.0025    0.0034   22057.4684    1.0000     1884.6094

  

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function lotto_not_two_consecutive_numbers(n=49,m=6)
    
    lotto ~ filldist(DiscreteUniform(1,n),m) 
    # Ensure unicity
    true ~ Dirac(sum([lotto[i] == lotto[j] for i in 1:m for j in i+1:m]) == 0)
    # Are there any consecutive numbers?
    prob ~ Dirac(sum([ abs(lotto[i]-lotto[j]) == 1 for i in 1:m for j in i+1:m]) == 0)
end

n = 49
m = 6
model = lotto_not_two_consecutive_numbers(n,m)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 40_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns[[:prob]])
# display(plot(chns))
