#=

    This is a port of the R2 model NoisyOr.cs

    Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

          n0    0.5170    0.5000     0.0158    0.0246   601.8992    1.0034       56.5482
          n4    0.4850    0.5000     0.0158    0.0258   628.8646    0.9991       59.0816
          n1    0.4580    0.4985     0.0158    0.0269   623.9486    1.0013       58.6198
         n21    0.4600    0.4986     0.0158    0.0218   566.2324    1.0061       53.1973
         n22    0.4430    0.4970     0.0157    0.0245   606.6086    0.9990       56.9907
         n33    0.4210    0.4940     0.0156    0.0201   661.3675    0.9993       62.1352
          n2    0.7090    0.4545     0.0144    0.0133   718.1745    1.0016       67.4722
         n31    0.4360    0.4961     0.0157    0.0242   558.9987    1.0076       52.5177
         n32    0.6020    0.4897     0.0155    0.0167   618.8106    0.9991       58.1370
          n3    0.8140    0.3893     0.0123    0.0170   576.4376    0.9994       54.1561

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function noisy_or() 
    n0 ~ Bernoulli(0.5);
    n4 ~ Bernoulli(0.5);

    if n0
        n1 ~ Bernoulli(0.8)

        n21 ~ Bernoulli(0.8)
    else
        n1 ~ Bernoulli(0.1)

        n21 ~ Bernoulli(0.1)
    end

    if n4
        n22 ~ Bernoulli(0.8)
        n33 ~ Bernoulli(0.8)
    else
        n22 ~ Bernoulli(0.1)
        n33 ~ Bernoulli(0.1)
    end

    n2 ~ Dirac(n21 || n22)

    if n1
        n31 ~ Bernoulli(0.8)
    else
        n31 ~ Bernoulli.(0.1)
    end

    if n2
        n32 ~ Bernoulli(0.8)
    else
        n32 ~ Bernoulli(0.1)
    end

    n3 ~ Dirac(n31 || n32 || n33)

end 

model = noisy_or()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)


display(chns)


