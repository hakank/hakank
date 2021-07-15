#=
  8 schools problem

  Cf ~/stan/eight_schools.stan
"""
           mean se_mean     sd   2.5%    25%    50%    75%  97.5%  n_eff   Rhat
mu         7.88    0.21   5.31  -2.91    4.6   7.91  11.12  18.27  658.0    1.0
tau        6.83    0.22   5.63   0.19   2.64   5.45   9.73  21.12  647.0   1.01
eta[1]     0.42    0.02   0.91  -1.52  -0.17   0.44   1.06   2.19 1843.0   1.01
eta[2]     0.01    0.02   0.89  -1.73  -0.57   0.01   0.59   1.78 2078.0    1.0
eta[3]    -0.19    0.02   0.89  -1.89   -0.8   -0.2   0.38   1.56 1837.0    1.0
eta[4]    -0.02    0.02   0.88  -1.78   -0.6  -0.02   0.54   1.66 1816.0    1.0
eta[5]    -0.37    0.02   0.88  -2.03  -0.97  -0.39   0.21   1.45 1511.0    1.0
eta[6]    -0.21    0.02   0.87  -1.96   -0.8  -0.24   0.38   1.57 1846.0    1.0
eta[7]     0.33    0.02   0.89  -1.48  -0.24   0.34   0.94    2.0 2000.0    1.0
eta[8]     0.08    0.02   0.91   -1.7  -0.53   0.08   0.69   1.85 2429.0    1.0
theta[1]   11.6    0.21   8.15  -1.93   6.18  10.61  15.69  30.96 1506.0    1.0
theta[2]   7.99    0.14   6.43  -4.29   3.86   7.86  11.95  20.89 2090.0    1.0
theta[3]   6.08    0.19   7.82 -12.72   2.22   6.59  10.81  20.56 1613.0    1.0
theta[4]   7.55    0.14   6.78  -6.74   3.51   7.69  11.79  21.31 2186.0    1.0
theta[5]   4.69    0.16   6.43  -9.19   0.82   5.34   9.12   16.0 1647.0    1.0
theta[6]   6.15    0.15   6.61  -8.13   2.34   6.55  10.63   18.5 1873.0    1.0
theta[7]  10.77    0.18   6.87  -1.07    6.2   9.98  14.85  25.87 1500.0    1.0
theta[8]   8.58     0.2   8.17  -7.99   3.87   8.24  12.88  26.77 1709.0    1.0
lp__      -4.74    0.11   2.63 -10.66  -6.35  -4.48  -2.85  -0.26  552.0    1.0
"""

  See ~/blog/8_shools.blog
      ~/webppl/8_shools.wppl

=#

# Fixing plot warnings GKS: character ignored due to unicode error
ENV["GKS_ENCODING"] = "utf-8"
using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)



include("jl_utils.jl")


@model function eight_schools(ys, sigmas)

    n = length(ys)

    # In contrast to stan, Turing requires a prior distribution.
    mu ~ Normal(10.0,10.0);
    tau ~ truncated(Normal(10.0,10.0),0.0,120.0)

    eta   =  tzeros(n)
    theta =  tzeros(n)
    sig   =  tzeros(n)
    for i in 1:n
        eta[i] ~ Normal(0.0,1.0)
        theta[i] = mu + tau * eta[i]
        # sig[i] ~ truncated(Normal(10,10),0,100) # s.e. of effect estimates
        sig[i] = sigmas[i]

        ys[i] ~ Normal(theta[i], sig[i]) # avg_likelihood
    end
end


ys     = [28.0,  8.0, -3.0,  7.0, -1.0,  1.0, 18.0, 12.0]
sigmas = [15.0, 10.0, 16.0, 11.0,  9.0, 11.0, 10.0, 18.0]
model = eight_schools(ys,sigmas)

num_chains = 4

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

chains = sample(model, MH(), 100_000)
# chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 10_000)

# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, IS(), 10_000)

# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 10_000)


# chains = sample(model, NUTS(1000,0.65), 1_000)
# chains = sample(model, HMC(0.1,5), 1_000)

display(chains)
display(plot(chains))
