#=
   Girl births  

   From Gelman et.al "Regression and Other Stories", 69f

   Part 1 (page 69)
   """
   The probability that a baby is a girl or boy is approximately 48.8% or 51.2%, respectively, 
   and these of births do not vary much across the world. Suppose that 400 babies are born in a 
   hospital in a given year. How many will be girls?
   """

   Part 2 (page 70)
   """
   Accounting for twins
   We can complicate the model in various ways. For example, there is a 1/125 chance that a 
   birth event results in fraternal twins, of which each has an approximate 49.5% chance of being a girl,  
   and a 1/300 chance of identical twins, which have an approximate 49.5% chance of being a pair of girls.
   """

Model 1:
Chains MCMC chain (10000×3×1 Array{Float64, 3}):

Log evidence      = 0.0
Iterations        = 1:1:10000
Number of chains  = 1
Samples per chain = 10000
Wall duration     = 1.79 seconds
Compute duration  = 1.79 seconds
parameters        = girl
internals         = lp, logevidence

Summary Statistics
  parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        girl   195.2312    9.9564     0.0996    0.1216   6737.7203    0.9999     3759.8886

Quantiles
  parameters       2.5%      25.0%      50.0%      75.0%      97.5% 
      Symbol    Float64    Float64    Float64    Float64    Float64 

        girl   176.0000   189.0000   195.0000   202.0000   215.0000

Credible interval for girl with mass 0.9: (178.000000..210.000000)
Credible interval for girl with mass 0.5: (186.000000..199.000000)


Model 2:
Chains MCMC chain (10000×6×1 Array{Float64, 3}):

Log evidence      = -4.462871026284194e-5
Iterations        = 1:1:10000
Number of chains  = 1
Samples per chain = 10000
Wall duration     = 5.06 seconds
Compute duration  = 5.06 seconds
parameters        = p, girl, identical_twins, fraternal_twins
internals         = lp, logevidence

Summary Statistics
       parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
           Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

  identical_twins     0.0027    0.0519     0.0005    0.0006   5793.9766    1.0000     1144.6023
  fraternal_twins     0.0109    0.1038     0.0010    0.0013   5777.4073    1.0000     1141.3290
                p     0.4881    0.0008     0.0000    0.0000   5843.3985    1.0000     1154.3656
             girl   195.6097   10.0797     0.1008    0.1158   6778.0922    0.9999     1339.0146

Quantiles
       parameters       2.5%      25.0%      50.0%      75.0%      97.5% 
           Symbol    Float64    Float64    Float64    Float64    Float64 

  identical_twins     0.0000     0.0000     0.0000     0.0000     0.0000
  fraternal_twins     0.0000     0.0000     0.0000     0.0000     0.0000
                p     0.4880     0.4880     0.4880     0.4880     0.4880
             girl   176.0000   189.0000   195.0000   202.0000   216.0000

Credible interval for girl with mass 0.9: (178.000000..211.000000)
Credible interval for girl with mass 0.5: (187.000000..200.000000)

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

# Part 1
# """
# The probability that a baby is a girl or boy is approximately 48.8% or 51.2%, respectively, 
# and these of births do not vary much across the world. Suppose that 400 babies are born in a 
# hospital in a given year. How many will be girls?
# """   
@model function girl_births1(n=400)
    girl ~ Binomial(n,0.488)
end

println("Model 1:")
n=400
model = girl_births1(n)

num_chains = 4

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 1_000)

display(chns)
# display(plot(chns))


credible_interval(chns,:girl,0.9)
credible_interval(chns,:girl,0.5)
println()

# Part 2
# """
# We can complicate the model in various ways. For example, there is a 1/125 chance that a 
# birth event results in fraternal twins, of which each has an approximate 49.5% chance of being a girl,  
# and a 1/300 chance of identical twins, which have an approximate 49.5% chance of being a pair of girls.
# """
@model function girl_births2(n=400)
    identical_twins ~ flip(1/300)
    fraternal_twins ~ flip(1/125)
    # Cannot be both identical_twins and fraternal_twins
    true ~ Dirac(identical_twins + fraternal_twins <= 1)
    # The probability of a girl:
    p ~ Dirac((identical_twins || fraternal_twins) ? 0.495 : 0.488)
    # Number of girls
    girl ~ Binomial(n,p)

end

println("\nModel 2:")
n=400
model = girl_births2(n)

num_chains = 4

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 1_000)

display(chns)
# display(plot(chns))


credible_interval(chns,:girl,0.9)
credible_interval(chns,:girl,0.5)
println()
