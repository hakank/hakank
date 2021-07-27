#=
  Linear regression.

  See ~/stan/linear_regression.R
"""
#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# alpha  4.03    0.00 0.13  3.79  3.95  4.03  4.12  4.28  1937    1
# beta   2.00    0.00 0.00  2.00  2.00  2.00  2.00  2.00  1975    1
# sigma  0.64    0.00 0.05  0.55  0.61  0.63  0.67  0.74  2179    1
# lp__  -4.85    0.03 1.20 -7.96 -5.42 -4.55 -3.96 -3.46  1594    1
"""

  See ~/blog/linear_regression.blog
      ~/webppl/linear_regression.wppl

  Note: I have problem with this model.
       - MH(), SMC, PG,give bad (or at least not great) rhat's
       - NUTS, HMC, etc throw errors
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function linear_regression(y)
    n = length(y)

    alpha ~ Normal(2,sqrt(2))
    beta  ~ Normal(2,sqrt(2))
    sigma ~ Gamma(2,1/2)


    # mu = Array{Float64}(undef, n)
    # mu = Vector{Float64}(undef, n)
    mu = tzeros(n)
    for i in 1:n
        mu[i] = alpha + beta*i
        y[i] ~ Normal(mu[i], sigma)
        # y[i] ~ Normal(alpha + beta*i, sigma)
    end

end


# Generated data (R):
# n = 100
# x <- 1:n
# y <- 4+2*x+rnorm(n,0,0.6)
#
# We are trying to restore these values:
#    alpha: 4.0
#    beta: 2.0
#    sigma: 0.6
#
#=
y = [5.983543,7.363850,9.001728,11.478878,14.753256,16.245218,
    18.758869,19.370087,23.417882,23.014029,25.900169,27.840894,
    29.350226,33.157245,34.046848,36.086745,38.871855,40.375736,
    42.633400,44.724727,45.845138,48.671348,50.828773,51.793457,
    54.159697,56.117874,57.545261,60.188977,62.063218,63.913408,
    66.528169,68.641637,70.790472,72.934844,73.421080,76.197684,
    78.432745,80.136338,81.058943,84.328133,85.410062,89.205925,
    89.229415,92.975393,93.531872,95.862181,97.878907,99.553297,
    102.068987,103.838061,106.568636,107.375497,110.583279,112.123316,
    113.890922,116.537840,117.444612,119.756863,123.567937,124.637554,
    125.430167,127.414399,129.799403,131.492455,134.129614,136.107248,
    138.823907,140.231367,142.460227,143.936719,146.620783,147.043767,
    150.176601,152.500756,152.473417,155.145638,158.793813,161.411058,
    163.030423,163.591843,165.696041,167.556486,169.848393,172.043780,
    173.372439,175.591097,177.409328,179.357999,181.339012,184.016542,
    186.234897,187.865354,190.316685,191.390721,193.391863,196.904094,
    197.551148,200.483832,201.944348,203.700001
    ]
=#
n = 100
x = 1:100
alpha = 4
beta = 2
sigma = 0.6
println("\nWe are trying to restore this: alpha($(alpha))+beta($(beta))*x+rand(Normal(0,sigma($(sigma))))\n")
# y = alpha+beta*x+rand(Normal(0,sigma))
y = alpha.+beta .* x .+rand(Normal(0,sigma),n)
# println("y:$y")
model = linear_regression(y)

num_chns = 4

# chns = sample(model, Prior(), MCMCThreads(), 10_000, num_chns)
chns = sample(model, MH(), 100_000)
# chns = sample(model, MH(
#                        # :alpha => Normal(2,sqrt(2)),
#                        # :beta => Normal(2,sqrt(2)),
#                        # :sigma => Gamma(2,2)
#                        ), MCMCThreads(), 40_000, num_chns)

# chns = sample(model, PG(15), MCMCThreads(), 1_000, num_chns)
# chns = sample(model, SMC(1_000), MCMCThreads(), 1_000, num_chns)
# chns = sample(model, IS(), 10_000)

# Both HMC and NUTS give the following error:
# ERROR: LoadError: TaskFailedException:
# TypeError: in typeassert, expected Float64, got a value of type ForwardDiff.Dual{Nothing,Float64,3}
# chns = sample(model, HMC(0.1,5), MCMCThreads(), 10_000, num_chns) # Error
# chns = sample(model, NUTS(0.65), MCMCThreads(), 10_000, num_chns) # Error

display(chns)
# display(plot(chns))
