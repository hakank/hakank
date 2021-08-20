#=

    This is a port of the R2 model TwoCoins.cs

    Output from the R2 model:
    ```
    [0] Mean: 0.339              firstCoin
    [0] Variance: 0.224303
    [1] Mean: 0.309              secondCoin
    [1] Variance: 0.213733
    Number of accepted samples = 847
    ```

    (The R2 model TwoCoinsTuple.cs is basically the same model as TwoCoins, except that 
     the return value is a tuple instead of the array [firstCoin,secondCoin], and is 
     not ported to Turing.jl.)


    This model:

  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

   firstCoin    0.3290    0.4701     0.0149    0.0269   487.5706    1.0010      156.9265
  secondCoin    0.3530    0.4781     0.0151    0.0189   636.1597    0.9994      204.7505
   bothHeads    0.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV, DataFrames
include("jl_utils.jl")


#
# Simulate skills and games
#
@model function two_coins() 
    firstCoin  ~ flip(0.5)
    secondCoin ~ flip(0.5)
    bothHeads ~ Dirac(firstCoin && secondCoin)

    true ~ Dirac(bothHeads == false)
end 


model = two_coins()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)

display(chns)

