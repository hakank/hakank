#=
  A/B test

  From http://rpubs.com/rasmusab/exercise_2_bayesian_ab_testing
  """
  Exercise 2: Bayesian A/B testing for Swedish Fish Incorporated with Stan
  Rasmus Bååth

  Swedish Fish Incorporated is the largest Swedish company delivering fish by mail order,
  but you probably already knew that. The marketing department have done a pilot study and
  tried two different marketing methods:

  A: Sending a mail with a colorful brochure that invites people to sign up for a one year
     salmon subscription.

  B: Sending a colorful brochure that invites people to sign up for a one year
     salmon subscription nd that includes a free salmon.

  The marketing department sent out 16 mails of type A and 16 mails of type B.
  Six Danes that received a mail of type A signed up for one year of salmon, and
  ten Danes that received a mail of type B signed up!

  The marketing department now wants to know, which method should we use, A or B?

  At the bottom of this document you’ll find a solution. But try yourself first!
  Question I: Build a Bayesian model in Stan that answers the question: What is the
  probability that method B is better than method A?
  """

  See ~/stan/ab_testing.R
      ~/blog/ab_testing.blog
      ~/psi/ab_testing.psi
      ~/webppl/ab_testing.wppl
=#

using Turing, StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

#=
ummary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64

       rateA    0.3893    0.1126     0.0003    0.0009   15774.8462    1.0004
       rateB    0.6121    0.1108     0.0003    0.0008   16943.7123    1.0001

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

       rateA    0.1839    0.3088    0.3857    0.4659    0.6183
       rateB    0.3847    0.5397    0.6153    0.6918    0.8148

mean(rateB > rateA): 0.91461875
mean(rateB - rateA): 0.22276908208667892
=#
@model function ab_testing(nA=16,nB=16,obsSA=6, obsSB=10)
    # nA = 16 # Number of sent mail
    # nB = 16

    # obsSA = 6; # Number of signments (observed)
    # obsSB = 10

    rateA ~ Beta(1,1) # priors
    rateB ~ Beta(1,1)

    # obsSA ~ Binomial(nA,rateA) # likelihood
    # obsSB ~ Binomial(nB,rateB)

    # Variant if we also want the numbers sA and sB
    sA ~ Binomial(nA,rateA) # likelihood
    sB ~ Binomial(nB,rateB)
    true ~ Dirac(sA == obsSA)
    true ~ Dirac(sB == obsSB)

    rateAGtRateB ~ Dirac(rateA > rateB)
    rateBGtRateA ~ Dirac(rateB > rateA)    
end

model = ab_testing(16,16,6,10)
# model = ab_testing(160,160,60,100)
num_chains = 4
# chains = sample(model, Prior(), MCMCThreads(), 1000, num_chains)
# chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, MH(), 40_000)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
chains = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, Gibbs(MH(:sA,:sB),NUTS(1000,0.65,:rateA,:rateB)), MCMCThreads(), 40_000, num_chains)

display(chains)
# display(plot(chains))

# Since Turing now has Dirac we don't need this hack...
df = DataFrame(chains);
println("mean(rateB > rateA): ", mean(df[!,"rateB"] .> df[!,"rateA"]))
println("mean(rateB - rateA): ", mean(df[!,"rateB"] .- df[!,"rateA"]))
