#=

    This is a port of the R2 model Hiv.cs

    with the datafiles 
    - hiv_hiv_y.csv
    - hiv_person.csv
    - hiv_time.csv 
    - hiv_num_persons.csv

    The R2 model output:
    '''
    ...
    [168] Mean: 0.169296        [muA1]
    [168] Variance: 0.206532

    [169] Mean: 0.12894         [muA2]
    [169] Variance: 0.469274

    [170] Mean: 55.8414         [sigmaA1]
    [170] Variance: 307.621

    [171] Mean: 49.4251         [sigmaA2]
    [171] Variance: 32.243

    [172] Mean: 44.0546         [sigmaY]
    [172] Variance: 228.657
    '''

    Using HMC(0.1,6):
Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        muA1   -0.4348    0.9159     0.0290    0.1427    42.9091    1.0093        0.4769
        muA2    0.2805    1.0354     0.0327    0.1446    56.6151    1.0393        0.6293
     sigmaA1   72.8204    2.6109     0.0826    0.1307   400.6981    0.9990        4.4539
     sigmaA2   29.7999    1.1906     0.0377    0.1017    88.2580    1.0322        0.9810
      sigmaY   71.2682    3.6674     0.1160    0.8280     3.7999    1.4849        0.0422

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

        muA1   -2.0894   -1.0887   -0.4788    0.2089    1.3453
        muA2   -1.6757   -0.4626    0.1953    0.9684    2.4151
     sigmaA1   67.2844   71.1223   72.8787   74.5292   77.6883
     sigmaA2   27.4074   28.9708   29.7822   30.6076   32.3003
      sigmaY   65.0158   68.4093   70.8732   74.1892   78.2249

138.288736 seconds (1.55 G allocations: 331.642 GiB, 15.40% gc time, 0.00% compilation time)


=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function hiv(hiv_y,hiv_person,hiv_time, hiv_num_persons) 
    n = length(hiv_y)

    muA1 ~ Normal(0,1)
    muA2 ~ Normal(0,1)

    sigmaA1 ~ Uniform(0,100)
    sigmaA2 ~ Uniform(0,100)

    a1 ~ filldist(Normal(muA1,sigmaA1),n)
    a2 ~ filldist(Normal(0.1*muA2,sigmaA2),n)

    sigmaY ~ Uniform(0,100)
    yHat = Vector{Real}(undef, n) 
    for i in 1:n
        yHat[i] = a1[hiv_person[i]] + a2[hiv_person[i]] * hiv_time[i]
        hiv_y[i] ~ Normal(yHat[i], sigmaY)
    end

end 

hiv_y = parse.(Float64,split(readline("hiv_y.csv"),","))
hiv_time  = parse.(Float64,split(readline("hiv_time.csv"),","))
hiv_num_persons = parse.(Int,split(readline("hiv_num_persons.csv"),","))
hiv_person = parse.(Int,split(readline("hiv_person.csv"),","))

println("len(y):$(length(hiv_y))")

model = hiv(hiv_y,hiv_person,hiv_time, hiv_num_persons[1])

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
# chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 1_000) # very slow
chns = sample(model, HMC(0.05,6), 1_000)


display(chns)

display(chns[[:muA1,:muA2,:sigmaA1,:sigmaA2,:sigmaY]])

