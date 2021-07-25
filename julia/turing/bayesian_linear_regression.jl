#=
  From BLOG example/bayesian-linear-regression.blog
  """
  A model for Bayesian linear regression
 
  @author yiwu
  @since 2015-07-02
  """


  Note: This is 0-based i.e.
     x y
     0 0
     1 1
     2 4
     3 6
     4 ?

  Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

           m    1.9616    0.4171     0.0042    0.0218   198.2693    1.0229       25.6327
           b   -0.1645    0.7370     0.0074    0.0321   351.3579    1.0083       45.4244
       sigma    0.9349    0.4702     0.0047    0.0309    86.5685    1.0103       11.1918
          y4    7.6895    1.6275     0.0163    0.0742   229.2941    1.0250       29.6437
     post[1]   -0.1513    1.2702     0.0127    0.0441   765.0960    1.0014       98.9135
     post[2]    1.8138    1.1771     0.0118    0.0365   910.5267    1.0006      117.7152
     post[3]    3.7861    1.1241     0.0112    0.0386   667.4734    1.0052       86.2926
     post[4]    5.7280    1.3455     0.0135    0.0475   517.3522    1.0120       66.8846
     post[5]    7.7025    1.6070     0.0161    0.0705   311.8366    1.0193       40.3150
     post[6]    9.6650    1.9211     0.0192    0.0894   276.0979    1.0201       35.6946
     post[7]   11.5449    2.1770     0.0218    0.1019   276.8155    1.0169       35.7874
     post[8]   13.5644    2.6050     0.0260    0.1283   236.7469    1.0168       30.6072
     post[9]   15.5463    2.9901     0.0299    0.1471   210.4787    1.0206       27.2112
    post[10]   17.4897    3.3944     0.0339    0.1688   230.5202    1.0202       29.8022
    post[11]   19.4406    3.7283     0.0373    0.1901   214.3265    1.0238       27.7087


   Cf ~/webppl/bayesian_linear_regression.wppl

=#

using Turing
include("jl_utils.jl")

@model function bayesian_linear_regression(y=[0,1,4,6])
    m ~ Normal(0,2)
    b ~ Normal(0,2)
    sigma ~ Gamma(1,1)
    for i in 0:length(y)-1
        y[i+1] ~ Normal(m * i + b, sigma)
    end

    # Posterior
    y4 ~ Normal(m * (4) + b, sigma)
    post = Vector{Real}(undef, n) 
    for i in 0:10
        post[i+1] ~ Normal(m * i + b, sigma)
    end
end

y = [0,1,4,6]
model = bayesian_linear_regression(y)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
#chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))



