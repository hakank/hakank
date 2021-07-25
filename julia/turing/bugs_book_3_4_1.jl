#=

  The BUGS Book
  Example 3.4.1 Three coins (page 45)
  """
  Suppose I have 3 coins in my pocket. The coins may be either fair, biased 3:1 in
  favour of heads, or 3:1 in favour of tails, but I don't know how many of
  each type there are among the 3 coins. I randomly select 1 coin att toss it once,
  observing a head. What is the posterior distribution of the probability of a head?
  """

  Note that JAGS has the somewhat weird structure of dcat etc.

  ~/jags/bugs_book_3_4_1.R
  """
  data {
    y <- 1
  } 

  model {

    y ~ dbern(theta.true)
    theta.true <- theta[coin]
    coin ~ dcat(p[])
    for(i in 1:3) {
      p[i] <- 1/3
      theta[i] <- 0.25*i
      coin.prob[i] <- equals(coin, i)
    }
  }

  Output:
               Mean     SD  Naive SE Time-series SE
coin         2.3320 0.7458 0.0026369      0.0026369
coin.prob[1] 0.1672 0.3732 0.0013194      0.0013194
coin.prob[2] 0.3335 0.4715 0.0016669      0.0016669
coin.prob[3] 0.4993 0.5000 0.0017678      0.0017590
theta.true   0.5830 0.1865 0.0006592      0.0006592
  """

  Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

          coin    2.3303    0.7518     0.0075    0.0101   4837.8060    1.0000      321.8124
      theta[1]    0.2500    0.0000     0.0000    0.0000         NaN       NaN           NaN
      theta[2]    0.5000    0.0000     0.0000    0.0000         NaN       NaN           NaN
      theta[3]    0.7500    0.0000     0.0000    0.0000         NaN       NaN           NaN
  coin_prob[1]    0.1720    0.3774     0.0038    0.0049   4883.5182    1.0000      324.8532
  coin_prob[2]    0.3257    0.4687     0.0047    0.0064   4998.4834    0.9999      332.5007
  coin_prob[3]    0.5023    0.5000     0.0050    0.0069   4914.2258    1.0000      326.8959
    theta_true    0.5826    0.1880     0.0019    0.0025   4837.8060    1.0000      321.8124
             y    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN


  Distributions of variable coin (num:0)
  3.00000 =>    4996  (0.499600)
  2.00000 =>    3355  (0.335500)
  1.00000 =>    1649  (0.164900)


  Cf ~/webppl/bugs_book_3_4_1.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_3_4_1()
    n = 3
    coin ~ DiscreteUniform(1,3) #  Select one coin. Note: JAGS has 1-based indexing.
    p = 1/3
    coin_prob = tzeros(3)
    theta = tzeros(3)
    for i in 1:n
        theta[i] ~ Dirac(0.25*i)
        coin_prob[i] ~ Dirac(coin == i) # Is this is the coin
    end

    theta_true ~ Dirac(theta[coin])
    y ~ flip(theta_true)

    true ~ Dirac(y == 1) #  We observe a head.

end

model = bugs_book_3_4_1()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(10_000), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,5), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :coin)
