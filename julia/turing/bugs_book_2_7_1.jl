#=

  The BUGS Book
  Example 2.7.1 Surgery (continued): prediction

  Cf ~/jags/bugs_book_2_7_1.R
  """
  model {
    theta ~ dbeta(3, 27) # prior distribution
    Y ~ dbin(theta, 20) # sampling distribution
    P6 <- step(Y - 5.5) # =1 if y >= 6, 0 otherwise
  }


Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64

       theta    0.0987    0.0524     0.0003    0.0011   1799.2552    1.0049
           y    1.9620    1.6057     0.0080    0.0367   1271.8025    1.0081

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

       theta    0.0193    0.0588    0.0900    0.1299    0.2219
           y    0.0000    1.0000    2.0000    3.0000    6.0000

Distributions of variable (num:40)
0.00000 =>   38931  (0.973275)
1.00000 =>    1069  (0.026725)
  4.176741 seconds (17.21 M allocations: 923.018 MiB, 28.68% gc time)

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_2_7_1()

    theta ~ Beta(3,27) # prior distribution
    y ~ Binomial(20, theta) # sampling distribution
    p6 ~ Dirac(y >= 6)

end

model = bugs_book_2_7_1()

num_chains = 4

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(), 100_000)
# chains = sample(model, MH(
#                        # :alpha => Normal(2,sqrt(2)),
#                        # :beta => Normal(2,sqrt(2)),
#                        # :sigma => Gamma(2,2)
#                        ), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)
# chains = sample(model, SMC(10_000), MCMCThreads(), 20_000, num_chains)
chains = sample(model, IS(), 10_000)

# Both HMC and NUTS give the following error:
# ERROR: LoadError: TaskFailedException:
# TypeError: in typeassert, expected Float64, got a value of type ForwardDiff.Dual{Nothing,Float64,3}
# chains = sample(model, HMC(0.1,5), MCMCThreads(), 10_000, num_chains) # Error
# chains = sample(model, NUTS(0.65), MCMCThreads(), 10_000, num_chains) # Error

display(chains)
# display(plot(chains))

show_var_dist_pct(chains,:p6 )
show_var_dist_pct(chains,:y )
# show_var_dist_pct(chains,:theta )
