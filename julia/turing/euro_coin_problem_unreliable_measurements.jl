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

   Continues on page 41:
   """
   Exercise 4.1. Suppose that instead of observing coin tosses directly, you measure
   the outcome using an instrument that is not always correct. Specifically, suppose
   there is a probability y that an actual heads is reported as tails, or actual tails re-
   ported as heads.

   Write a class that estimates the bias of a coin given a series of outcomes and the
   value of y .
   How does the spread of the posterior distribution depend on y ?
   """

   mean(prob): 0.5213151098904925
   Distributions of variable probLt0_5 (num:0)
   0.00000 =>    7020  (0.702000)
   1.00000 =>    2980  (0.298000)

   Distributions of variable error (num:0)
   0.00000 =>    7051  (0.705100)
   1.00000 =>    2949  (0.294900)


   Cf euro_coin_problem.jl
   Cf ~/blog/euro_coin_unreliable_measurements.blog
      ~/webppl/euro_coin_unreliable_measurements.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function euro_coin_problem_unreliable_measurements()
    n = 250
    
    # Probability of throwing head
    prob ~ Beta(2,2)
    
    # We measure incorrect with probability 0.2
    error ~ flip(0.2)
    
    throwCoin ~ filldist(flip(prob),n)
    
    coin = tzeros(n)
    for i in 1:n
        coin[i] ~ Dirac(error == true ? 1-throwCoin[i] : throwCoin[i])
    end
    
    sum250 ~ Dirac(sum([coin[i] == true ? 1 : 0 for i in 1:n]))
    true ~ Dirac(sum250 == 140)
    probLt0_5 ~ Dirac(prob < 0.5)
    
end

model = euro_coin_problem_unreliable_measurements()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

println("mean(prob): ", mean(chns[:prob]))
show_var_dist_pct(chns, :probLt0_5)
show_var_dist_pct(chns, :error)

    
