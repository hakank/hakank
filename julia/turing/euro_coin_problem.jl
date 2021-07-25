#=

   The Euro coin problem.

   From Think Bayes, page 33ff
   """
   A statistical statement appeared in "The Guardian" on Friday January 4, 2002:
      When spun on edge 250 times, a Belgian one-euro coin
      came up heads 140 times and tails 110. 'It looks very
      suspicious to me,' said Barry Blight, a statistics lecturer
      at the London School of Economics. 'If the coin were
      unbiased, the chance of getting a result as extreme as
      that would be less than 7%.'

   But do these data give evidence that the coin is biased rather than fair?
   """

  Model 1
  mean(prob): 0.5639764325445531
  Distributions of variable probLt0_5 (num:0)
  0.00000 =>    9896  (0.989600)
  1.00000 =>     104  (0.010400)

  Distributions of variable probGt0_5 (num:0)
  1.00000 =>    9896  (0.989600)
  0.00000 =>     104  (0.010400)

  Model 2
  Summary Statistics
  parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        prob     0.5496    0.0296     0.0003    0.0026     52.7368    1.0154        6.8901
       heads   139.9668    1.8488     0.0185    0.0332   2384.8100    1.0002      311.5770
   probLt0_5     0.0006    0.0245     0.0002    0.0006   1544.1256    1.0005      201.7410
   probGt0_5     0.9994    0.0245     0.0002    0.0006   1544.1256    1.0005      201.7410


   Cf ~/blog/euro_coin_problem.blog
      ~/psi/euro_coin_problem.blog
      ~/webppl/euro_coin_problem.jl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function euro_coin_problem1()
    n = 250
    
    # Probability of throwing head
    prob ~ Beta(2,2) # Uniform(0,1)
    coin ~ filldist(flip(prob),n)
    
    sum250 ~ Dirac(sum([coin[i] == true ? 1 : 0 for i in 1:n]))
    true ~ Dirac(sum250 == 140)

    probLt0_5 ~ Dirac(prob < 0.5)
    probGt0_5 ~ Dirac(prob > 0.5)
    
end

# Simpler and faster
@model function euro_coin_problem2()
    prob ~ Beta(2,2)
    heads ~ Binomial(250,prob)
    
    true ~ Dirac(heads==140)

    probLt0_5 ~ Dirac(prob < 0.5)
    probGt0_5 ~ Dirac(prob > 0.5)

end


println("Model 1")
model = euro_coin_problem1()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))

println("mean(prob): ", mean(chns[:prob]))
show_var_dist_pct(chns, :probLt0_5)
show_var_dist_pct(chns, :probGt0_5)
    

println("\nModel 2")
model = euro_coin_problem2()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :probLt0_5)
show_var_dist_pct(chns, :probGt0_5)

