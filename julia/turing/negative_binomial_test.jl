#=

  Test of negative binomial.

  From BLOG documentation:
  """
  NegativeBinomial distribution generates the number of failures until the rth success
  in a sequence of independent Bernoulli trials each with probability of success p.

  Example: The following code defines a random function symbol x distributed according to a
  Negative Binomial distribution with probability of success p = 0.8 and number of failures r = 2.
  random Integer x ~ NegativeBinomial(2, 0.8);
  """


  https://stattrek.com/probability-distributions/negative-binomial.aspx
  """
  Bob is a high school basketball player. He is a 70% free throw shooter. That means his probability
  of making a free throw is 0.70. During the season, what is the probability that Bob makes his
  third free throw on his fifth shot?

  Solution: This is an example of a negative binomial experiment. The probability of success (P)
  is 0.70, the number of trials (x) is 5, and the number of successes (r) is 3.

  To solve this problem, we enter these values into the negative binomial formula.

  b*(x; r, P) = x-1Cr-1 * Pr * Qx - r
  b*(5; 3, 0.7) = 4C2 * 0.73 * 0.32
  b*(5; 3, 0.7) = 6 * 0.343 * 0.09 = 0.18522

  Thus, the probability that Bob will make his third successful free throw on his fifth shot is 0.18522.
  """

  The distribution of y give a fair estimation
  0 =>   55089  0.344306
  1 =>   49144  0.307150
  2 =>   29557  0.184731 <---
  3 =>   14803  0.092519
  4 =>    6683  0.041769
  5 =>    2829  0.017681
  6 =>    1157  0.007231
  7 =>     476  0.002975
  8 =>     166  0.001038
  9 =>      55  0.000344
 10 =>      30  0.000188
 11 =>      10  0.000063
 12 =>       1  0.000006


  Cf ~/blog/negative_binomial_test.blog
     ~/webppl/negative_binomial_test.wppl

=#

# using Memoization
using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
using Printf
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

# Note: This does not really require Turing.
# The following is easier...
#=
# julia> rand(NegativeBinomial(3,0.7),100000)|>make_hash|>sort
OrderedCollections.OrderedDict{Any,Any} with 13 entries:
  0  => 34214
  1  => 30817
  2  => 18522
  3  => 9322
  4  => 4238
  5  => 1740
  6  => 694
  7  => 302
  8  => 96
  9  => 37
  10 => 11
  11 => 6
  13 => 1
=#
@model function negative_binomial_test()
  y ~ NegativeBinomial(3,0.7)
end


model = negative_binomial_test()

num_chns = 4


# chns = sample(model, Prior(), MCMCThreads(), 40_000, num_chns)
#
# chns = sample(model, MH(), 40_000)
chns = sample(model, MH(), MCMCThreads(), 40_000, num_chns)

# chns = sample(model, PG(20), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, PG(20), 1_000)

# chns = sample(model, IS(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, SMC(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, SMC(), 10_000)

# chns = sample(model, NUTS(1000,0.65), 40_000)
# chns = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chns)

# chns = sample(model, Gibbs(MH(:n),NUTS(1000,0.65,:y)), MCMCThreads(), 1_000, num_chns)
# chns = sample(model, Gibbs(MH(:n),NUTS(10,0.65,:y)), 1_000)

display(chns)

show_var_dist(chns,:y)
