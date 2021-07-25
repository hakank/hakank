#=
   From https://reference.wolfram.com/language/ref/BinomialDistribution.html
   """
   Compute the probability that there are between 60 and 80 heads in 100 coin flips.
   ... and an unfair coin (p:0.6)
   """

   From Mathematica: 
    - fair coin 0.028444
    - unfair coin: 0.543289

   Distributions of variable fair_coin_60_80 (num:0)
   0.00000 =>    9707  (0.970700)
   1.00000 =>     293  (0.029300)

   Distributions of variable unfair_coin_60_80 (num:0)
   1.00000 =>    5455  (0.545500)
   0.00000 =>    4545  (0.454500)

   Cf ~/webppl/binomial_coin.wppl

=#

using Turing
include("jl_utils.jl")

@model function binomial_coin()
    n = 100
    
    head ~ filldist(flip(0.5),n)
    headUnfair ~ filldist(flip(0.6),n)
    
    numHeads ~ Dirac(sum(head))
    numHeadsUnfair ~ Dirac(sum(headUnfair))

    fair_coin_60_80 ~ Dirac(numHeads >= 60 && numHeads <= 80)
    unfair_coin_60_80 ~ Dirac(numHeadsUnfair >= 60 && numHeadsUnfair <= 80)

end

model = binomial_coin()

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

show_var_dist_pct(chs,:fair_coin_60_80)
show_var_dist_pct(chs,:unfair_coin_60_80)
