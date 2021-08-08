#=
    Logistic regresiion
    Inspired by WebPPL's examples/logisticRegression.wppl

    Cf logistic_regression.jl 

    In this model we actually use logicstic().

      parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
          Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

               m    1.0285    0.5627     0.0178    0.0254    687.0390    1.0014      166.1119
               b    0.1912    0.9294     0.0294    0.0342    705.2296    0.9995      170.5100
  labels_post[1]    0.0180    0.1330     0.0042    0.0044    821.3831    0.9990      198.5936
  labels_post[2]    0.0610    0.2395     0.0076    0.0087    873.0023    0.9990      211.0741
  labels_post[3]    0.8440    0.3630     0.0115    0.0075    858.1636    0.9993      207.4864
  labels_post[4]    0.9740    0.1592     0.0050    0.0048    847.3995    0.9992      204.8838
  labels_post[5]    0.9880    0.1089     0.0034    0.0029   1028.5041    0.9993      248.6712

=#

using Turing, StatsPlots, DataFrames
using StatsFuns

include("jl_utils.jl")

@model function logistic_regression2(x, labels)
    m ~ Normal(0, 1)
    b ~ Normal(0, 1)

    n = length(x)
    v = tzeros(n)
    for i in 1:n
        v[i] = logistic(b + m*x[i])
        labels[i] ~ Bernoulli(v[i])
        
    end

    # Post predictive of labels
    labels_post = tzeros(n)
    for i in 1:n
        labels_post[i] ~ Bernoulli(v[i])
    end
end

xs = [-10, -5, 2, 6, 10]
labels = [0, 0, 1, 1, 1]
model = logistic_regression2(xs,labels)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
