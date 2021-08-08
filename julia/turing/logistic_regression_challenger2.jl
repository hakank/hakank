#=
   Logistic regression - Challenger

   See ~/jags/logistic_regression_challenger.R
   """
   model {
     for (i in 1:N){
       y[i] ~ dbern(p[i])
       p[i] <- 1 / (1 + exp(-z[i]))
       z[i] <- w0 + w1 * x[i]
     }
     w0 ~ dnorm(0, .001)
     w1 ~ dnorm(0, .0001)

   Output:
      Mean     SD  Naive SE Time-series SE
w0 16.8900 7.7265 0.0122167       0.290180
w1 -0.2602 0.1135 0.0001794       0.004232

2. Quantiles for each variable:

     2.5%     25%     50%     75%    97.5%
w0  4.131 11.3621 16.1240 21.5029 34.27954
w1 -0.516 -0.3279 -0.2488 -0.1789 -0.07332
   """

  From https://www.zinkov.com/posts/2012-06-27-why-prob-programming-matters/
  "Logistic Regression"
  """
  Logistic Regression can be seen as a generalization of Linear Regression where the output is 
  transformed
  to lie between 0 and 1. This model only differs from the previous one by a single line, illustrating that
  adding this complexity does not require starting from scratch. The point with probabilistic programming
  is you are able to explore slightly more complex models very easily.
  """


  From https://www.stat.ubc.ca/~bouchard/courses/stat520-sp2014-15/lecture/2015/02/27/notes-lecture3.html
  x = 66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58
  y = 1,0,1,1,1,1,1,1,0,0,0,1,1,0,1,1,1,1,1,1,0,1,0

  Compared to logistic_regression_challenger.jl here we use the logicstic function instead.


    Summary Statistics
    parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

            w0   -9.1270    5.3364     0.1688    1.1745    16.5012    1.0096        2.1562
            w1    0.1893    0.4241     0.0134    0.0245   167.3714    1.0025       21.8700
    y_post[1]    0.7570    0.4291     0.0136    0.0251   578.5337    1.0030       75.5957
    y_post[2]    0.8380    0.3686     0.0117    0.0158   653.2435    0.9990       85.3578
    y_post[3]    0.8240    0.3810     0.0120    0.0177   548.3477    0.9990       71.6513
    y_post[4]    0.8140    0.3893     0.0123    0.0171   519.4787    1.0042       67.8791
    y_post[5]    0.7880    0.4089     0.0129    0.0174   827.3196    1.0005      108.1040
    y_post[6]    0.8680    0.3387     0.0107    0.0117   499.8656    0.9991       65.3163
    y_post[7]    0.9100    0.2863     0.0091    0.0125   418.0910    1.0039       54.6310
    y_post[8]    0.8440    0.3630     0.0115    0.0203   380.3478    0.9995       49.6992
        ⋮           ⋮         ⋮         ⋮          ⋮         ⋮          ⋮           ⋮
                                                                            15 rows omitted

    Posterior predictive:
    1: true: 1 pred: 1 (0.757)
    2: true: 0 pred: 1 (0.838)!
    3: true: 1 pred: 1 (0.824)
    4: true: 1 pred: 1 (0.814)
    5: true: 1 pred: 1 (0.788)
    6: true: 1 pred: 1 (0.868)
    7: true: 1 pred: 1 (0.91)
    8: true: 1 pred: 1 (0.844)
    9: true: 0 pred: 0 (0.415)
    10: true: 0 pred: 1 (0.673)!
    11: true: 0 pred: 1 (0.83)!
    12: true: 1 pred: 1 (0.935)
    13: true: 1 pred: 1 (0.77)
    14: true: 0 pred: 0 (0.297)
    15: true: 1 pred: 1 (0.767)
    16: true: 1 pred: 1 (0.911)
    17: true: 1 pred: 1 (0.853)
    18: true: 1 pred: 1 (0.952)
    19: true: 1 pred: 1 (0.922)
    20: true: 1 pred: 1 (0.949)
    21: true: 0 pred: 1 (0.92)!
    22: true: 1 pred: 1 (0.923)
    23: true: 0 pred: 0 (0.437)
    num_correct: 19 / 23 (82.6086956521739%)

  Cf logistic_regression_challenger.jl 
      ~/blog/logistic_regression-challenger.blog
     ~/webppl/logistic_regression-challenger.wppl

=#

using Turing, StatsPlots

include("jl_utils.jl")

@model function logistic_regression_challenger2(xs,ys)
    n = length(xs)
    w0 ~ Normal(0, 10)
    w1 ~ Normal(0, 10)

    p = tzeros(n)    
    for i in 1:n
        p[i] = logistic(w0 + w1 * xs[i])
        ys[i] ~ Bernoulli(p[i])
    end

    y_post = tzeros(n)
    for i in 1:n
        y_post[i] ~ Bernoulli(p[i])
    end
end

xs = [66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58];
# ys = [true,false,true,true,true,true,true,true,false,false,false,true,true,false,true,true,true,true,true,true,false,true,false];
ys = [1,0,1,1,1,1,1,1,0,0,0,1,1,0,1,1,1,1,1,1,0,1,0];
#     1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3

model = logistic_regression_challenger2(xs,ys)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))

# display(plot(chns[[:w0,:w1]]) # quite bad!

println("Posterior predictive:")
num_correct = 0
n = length(ys)
for i in 1:n
    pred1 = mean(chns["y_post[$i]"])
    pred2 = round(Int64,pred1)
    c = "!"
    if pred2 == ys[i] 
        global num_correct += 1
        c = ""
    end
    println("$i: true: $(ys[i]) pred: $pred2 ($pred1)$c" )
end
println("num_correct: $num_correct / $n ($(100*num_correct/n)%)")