#=

    This is a port of the R2 model CoinBias.cs 

    Output from the R2 model:
    ```
    Mean: 0.421294
    Variance: 0.0162177
    Number of accepted samples = 692
    ```

    This model:
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        bias    0.4166    0.1360     0.0014    0.0020   5021.6561    1.0000     2276.3627
   
=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function coin_bias(x) 
    n = length(x)
    # Beta(2,5) has mean about 0.2855
    bias ~ Beta(2,5)
    x ~ filldist(Bernoulli(bias),n)
end 


x = parse.(Int,split(readline("coin_bias.txt"),","))
println("x:$x")
model = coin_bias(x)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)

show_var_dist_pct(chns,:bias,20)