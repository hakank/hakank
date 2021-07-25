#=
  Biased coin

  From cplint: http://cplint.eu/example/inference/coin.pl
  """
  Throwing a coin with uncertainty on its fairness, from
  J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated
  disjunctions. In International Conference on Logic Programming,
  volume 3131 of LNCS, pages 195-209. Springer, 2004.
  """

  Model 1: Probability of tossing a head (no observation)
  Distributions of variable coinHead (num:0)
  1.00000 =>    5049  (0.504900)
  0.00000 =>    4951  (0.495100)

  Model 2: Probability of tossing a head given that we know it's biased
  Distributions of variable coinHead (num:0)
  1.00000 =>    5712  (0.571200)
  0.00000 =>    4288  (0.428800)

  Cf ~/cplint/coin.pl
     ~/blog/biased_coin.blog
     ~/webppl/biased_coin.wppl

=#

using Turing
include("jl_utils.jl")

@model function biased_coin(experiment="observe biased")
    fair = 1
    biased = 2

    coinType ~ Categorical([0.9,0.1]) # fair,biased
    head = 1
    tail = 2
    coinResult ~ coinType == fair ? Categorical([0.5,0.5]) :
            Categorical([0.6,0.4])

    if experiment == "observe biased"
        true ~ Dirac(coinType == biased)
    end

    coinHead ~ Dirac(coinResult == head)
    coinTail ~ Dirac(coinResult == tail)

end

println("Model 1: Probability of tossing a head (no observation)")
model = biased_coin("no observation")

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

# display(chs)
# display(plot(chs))

show_var_dist_pct(chs,:coinHead)


println("\nModel 2: Probability of tossing a head given that we know it's biased")
model = biased_coin("observe biased")

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

# display(chs)
# display(plot(chs))

show_var_dist_pct(chs,:coinHead)

