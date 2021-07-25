#=
   AR(1) in Turing.jl

   From Stan Users Guide, section 3.1 (page 51) "Autoregressive Models"
   (Though this is a port of my WebPPL model ar1.wppl)

   From the R model
   """
  Result:
          mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
   alpha  0.73    0.00 0.10  0.54  0.67  0.73  0.80  0.92 14499    1
   beta   0.16    0.00 0.10 -0.04  0.09  0.16  0.23  0.36 14601    1
   sigma  0.35    0.00 0.03  0.31  0.34  0.35  0.37  0.41 19812    1
   lp__  53.17    0.01 1.25 49.90 52.60 53.50 54.09 54.61 12256    1
   """

  Using Normal, Normal, and Normal (for alpha, beta, sigma)
  """
  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

       alpha    0.7332    0.0943     0.0009    0.0015   4013.6785    1.0001      388.6211
        beta    0.1554    0.1002     0.0010    0.0016   3916.5955    1.0001      379.2211
       sigma    0.3520    0.0259     0.0003    0.0004   5418.8333    1.0002      524.6740

  Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

       alpha    0.5492    0.6695    0.7329    0.7962    0.9212
        beta   -0.0423    0.0878    0.1546    0.2228    0.3491
       sigma    0.3060    0.3339    0.3502    0.3683    0.4076
  """

  Using Normal, Normal, and Truncated(Gamma(2,2), 0,Inf) (for alpha, beta, sigma)
  """
  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

       alpha    0.7279    0.0977     0.0010    0.0015   3948.5885    1.0000      757.1598
        beta    0.1612    0.1038     0.0010    0.0016   4001.2383    1.0001      767.2557
       sigma    0.3532    0.0258     0.0003    0.0003   5222.2375    0.9999     1001.3878

  Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

       alpha    0.5348    0.6627    0.7290    0.7928    0.9226
        beta   -0.0428    0.0929    0.1602    0.2296    0.3664
       sigma    0.3064    0.3351    0.3517    0.3693    0.4073

   """


   Cf ~/stan/ar1.R and ~/stan/ar1.stan
      ~/webppl/ar1.wppl

=#

using Turing
include("jl_utils.jl")

@model function ar1(ys)
    n = length(ys)

    # Uninformative priors
    # alpha ~ Uniform(-2,2)
    # beta ~ Uniform(-2,2)
    # sigma ~ Uniform(0,2)

    # Informative priors
    alpha ~ Normal(2,2)
    beta ~ Normal(2,2)
    sigma ~ Truncated(Gamma(2,2), 0, Inf)

    for i in 1:n
        if i == 1
            ys[i] ~ Normal(alpha + beta, sigma)
        else
            ys[i] ~ Normal(alpha + beta * ys[i-1], sigma)
        end
    end

end

ys = [0.705429, 1.43062, 0.618161, 0.315107, 1.09993, 1.49022, 0.690016, 0.587519, 0.882984,
      1.0278, 0.998615, 0.878366,  1.17405,  0.532718, 0.486417, 1.13685, 1.32453, 1.3661,
      0.914368, 1.07217, 1.1929, 0.418664, 0.889512, 1.47218, 1.13471, 0.410168, 0.639765,
      0.664874, 1.12101, 1.22703, -0.0931769, 0.4275, 0.901126, 1.01896, 1.27746, 1.17844,
      0.554775, 0.325423, 0.494777, 1.05813, 1.10177, 1.11225, 1.34575, 0.527594, 0.323462,
      0.435063, 0.739342, 1.05661, 1.42723, 0.810924, 0.0114801, 0.698537, 1.13063, 1.5286,
      0.968813, 0.360574, 0.959312, 1.2296, 0.994434, 0.59919, 0.565326, 0.855878, 0.892557,
      0.831705, 1.31114, 1.26013, 0.448281, 0.807847, 0.746235, 1.19471, 1.23253, 0.724155,
      1.1464, 0.969122, 0.431289, 1.03716, 0.798294, 0.94466, 1.29938, 1.03269, 0.273438,
      0.589431, 1.2741, 1.21863, 0.845632, 0.880577, 1.26184, 0.57157, 0.684231, 0.854955,
      0.664501, 0.968114, 0.472076, 0.532901, 1.4686, 1.0264, 0.27994, 0.592303, 0.828514,
      0.625841]

model = ar1(ys)
num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
# chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)
# chs = sample(model, SGLD(), 10_000)
chs = sample(model,NUTS(), 10_000)

display(chs)
# display(plot(chs))

