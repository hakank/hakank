#=
  
  From https://reference.wolfram.com/language/ref/BinomialDistribution.html
  """
  A basketball player has a free-throw percentage of 0.75. 

  Find the probability that the player hits 2 out of 3 free throws in a game.
  [free_throwIs2]
    Answer: 0.421875

  Find the probability that the player hits the last 2 of 5 free throws. 
  [q1]
     Answer: 0.00878906
   

  Find the expected number of hits in a game with n free throws. 
  [q2]
     Answer: 0.75 n
  """

  This model don't find the answer on q1 since the probability is too small...

  q2:  0.7504 


  Distributions of variable free_throwIs2 (num:0)
  0.00000 =>   57552  (0.575520)
  1.00000 =>   42448  (0.424480)
  Distributions of variable q1 (num:0)
  0.00000 =>  100000  (1.000000)
  Distributions of variable q2 (num:0)
  0.80000 =>   27864  (0.278640)
  0.70000 =>   25049  (0.250490)
  0.90000 =>   18829  (0.188290)
  0.60000 =>   14636  (0.146360)
  0.50000 =>    5906  (0.059060)
  1.00000 =>    5803  (0.058030)
  0.40000 =>    1606  (0.016060)
  0.30000 =>     269  (0.002690)
  0.20000 =>      37  (0.000370)
  0.10000 =>       1  (0.000010)


  Cf ~/webppl/binomial_basketball.wppl

=#

using Turing
include("jl_utils.jl")

@model function binomial_basketball()

    p = 0.75
    n = 2
    
    # Find the probability that the player hits 2 out of 3 free throws in a game.
    free_throw ~ Binomial(3,p)
    
    free_throwIs2 ~ Dirac(free_throw == 2)

    # Find the probability that the player hits the last 2 of 5 free throws.
    free_throw2a ~ Binomial(3,p) # This should be 0
    free_throw2b ~ Binomial(2,p) # This should be 2
    
    # Find the expected number of hits in a game with n free throws.
    free_throw3 ~ Binomial(10,p)
    
    q1 ~ Dirac(free_throw2a == 0 & free_throw2b == 2)
    q2 ~ Dirac(free_throw3 / 10.0)

end

model = binomial_basketball()

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 40_000)
# chs = sample(model, IS(), 50_000)
chs = sample(model, SMC(), 100_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

show_var_dist_pct(chs,:free_throwIs2)
show_var_dist_pct(chs,:q1)
show_var_dist_pct(chs,:q2)
