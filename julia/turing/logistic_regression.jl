#=
   Logistic regresiion
   Inspired by WebPPL's examples/logisticRegression.wppl

   Summary Statistics (for PG(15))
      parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
          Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

               m     1.0844    0.5870     0.0059    0.0077   6361.4279    1.0009      153.4390
               b     0.1315    0.9292     0.0093    0.0111   6431.4331    0.9999      155.1275
           sigma     0.8779    0.8513     0.0085    0.0107   6452.7250    0.9999      155.6411
            y[1]   -10.7253    6.0611     0.0606    0.0801   6408.9867    1.0008      154.5861
            y[2]    -5.3599    3.2295     0.0323    0.0439   6465.7579    1.0007      155.9555
            y[3]     2.5165    1.7809     0.0178    0.0200   6562.8112    1.0010      158.2964
            y[4]     6.6892    3.7534     0.0375    0.0454   6516.1303    1.0008      157.1705
            y[5]    11.0158    5.9711     0.0597    0.0746   6263.6413    1.0010      151.0804
  labels_post[1]     0.0163    0.1266     0.0013    0.0013   8339.5233    0.9999      201.1511
  labels_post[2]     0.0552    0.2284     0.0023    0.0026   8486.3592    1.0001      204.6928
  labels_post[3]     0.8459    0.3611     0.0036    0.0041   8013.9274    1.0001      193.2977
  labels_post[4]     0.9707    0.1687     0.0017    0.0018   8543.2209    1.0002      206.0643
  labels_post[5]     0.9873    0.1120     0.0011    0.0012   9349.4013    0.9999      225.5096

  The labels_post are the posterior predictive and are fairly close to labels values: [0, 0, 1, 1, 1]

=#

using Turing, StatsPlots, DataFrames

include("jl_utils.jl")

@model function logistic_regression(x, labels)
    m ~ Normal(0, 1)
    b ~ Normal(0, 1)
    sigma ~ Gamma(1, 1)

    n = length(x)
    y = tzeros(n)
    sigmoid = tzeros(n)
    for i in 1:n
        y[i] ~ Normal(m * x[i] + b, sigma)
        sigmoid[i] = 1 / (1 + exp(-1 * y[i]))
        labels[i] ~ Bernoulli(sigmoid[i])
    end

    # Post predictive of labels
    labels_post = tzeros(n)
    for i in 1:n
        labels_post[i] ~ Bernoulli(sigmoid[i])
    end
end

xs = [-10, -5, 2, 6, 10]
labels = [0, 0, 1, 1, 1]
model = logistic_regression(xs,labels)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
