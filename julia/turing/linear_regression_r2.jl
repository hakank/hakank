#=

    This is a port of the R2 model LinearRegression.cs

    with the datafiles:
    - linear_regression_r2_x.csv
    - linear_regression_r2_y.csv
    - linear_regression_r2_x1.csv
    - linear_regression_r2_y1.csv


    * Using linear_regression_r2_x1.csv and linear_regression_r2_y1.csv:

  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

           a   -0.4114    0.0001     0.0000    0.0000   329.9865    1.0070       31.0751
           b    3.0589    0.0806     0.0025    0.0045   273.3509    1.0100       25.7417
    invNoise    1.2838    0.0280     0.0009    0.0012   341.9381    1.0049       32.2006


    * Using linear_regression_r2_x1.csv and linear_regression_r2_y1.csv:

  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           a   -0.5363    0.0370     0.0012    0.0008   1937.8035    0.9998      793.8564
           b    3.0138    0.0427     0.0014    0.0009   1209.1662    0.9990      495.3569
    invNoise    1.4142    0.0314     0.0010    0.0007   1540.4202    0.9995      631.0611

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function linear_regression_r2(x,y) 
    a ~ Normal(0,1)
    b ~ Normal(5.0,1.82574185835055371152)

    invNoise ~ Gamma(1,1)
    mu = a * x .+ b
    y ~ MvNormal(mu, invNoise)
end 

x = parse.(Float64,split(readline("linear_regression_r2_x.csv"),","))
y = parse.(Float64,split(readline("linear_regression_r2_y.csv"),","))

# x = parse.(Float64,split(readline("linear_regression_r2_x1.csv"),","))
# y = parse.(Float64,split(readline("linear_regression_r2_y1.csv"),","))

model = linear_regression_r2(x,y)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
# chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)


display(chns)


