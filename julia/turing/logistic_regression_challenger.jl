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

Summary Statistics
  parameters       mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol    Float64   Float64    Float64   Float64    Float64   Float64       Float64 

          w0   -33.3287   27.6137     0.2761    2.7552    22.0381    1.0003        0.3419
          w1     0.5239    0.4022     0.0040    0.0401    22.0718    0.9999        0.3424
   y_post[1]     0.6917    0.4618     0.0046    0.0230    80.9484    1.0391        1.2558
   y_post[2]     0.9368    0.2433     0.0024    0.0089   159.7127    1.0322        2.4777
   y_post[3]     0.9186    0.2735     0.0027    0.0074   304.2265    1.0235        4.7195
   y_post[4]     0.8776    0.3278     0.0033    0.0077   380.5956    1.0229        5.9043
   y_post[5]     0.8053    0.3960     0.0040    0.0130   193.3661    1.0251        2.9997
   y_post[6]     0.9399    0.2377     0.0024    0.0114    93.7454    1.0461        1.4543
   y_post[7]     0.9394    0.2386     0.0024    0.0116    90.6911    1.0474        1.4069
   y_post[8]     0.9356    0.2455     0.0025    0.0088   166.6864    1.0323        2.5858
   y_post[9]     0.3533    0.4780     0.0048    0.0369    36.9957    0.9999        0.5739
  y_post[10]     0.4858    0.4998     0.0050    0.0415    30.6274    1.0526        0.4751
  y_post[11]     0.9357    0.2453     0.0025    0.0083   193.5760    1.0295        3.0030
  y_post[12]     0.9158    0.2777     0.0028    0.0172    52.1763    1.0862        0.8094
  y_post[13]     0.7979    0.4016     0.0040    0.0139   171.6327    1.0287        2.6626
  y_post[14]     0.2547    0.4357     0.0044    0.0354    32.1260    1.0606        0.4984
  y_post[15]     0.7999    0.4001     0.0040    0.0133   190.0694    1.0279        2.9486
  y_post[16]     0.9272    0.2598     0.0026    0.0148    64.1946    1.0679        0.9959
  y_post[17]     0.9384    0.2404     0.0024    0.0081   203.3125    1.0277        3.1540
  y_post[18]     0.8981    0.3025     0.0030    0.0208    42.3442    1.1074        0.6569
  y_post[19]     0.9268    0.2605     0.0026    0.0148    63.1168    1.0693        0.9791
  y_post[20]     0.9092    0.2873     0.0029    0.0184    48.8488    1.0916        0.7578
  y_post[21]     0.9295    0.2560     0.0026    0.0142    65.7403    1.0655        1.0198
  y_post[22]     0.9262    0.2615     0.0026    0.0149    61.2013    1.0709        0.9494
  y_post[23]     0.3722    0.4834     0.0048    0.0377    36.3094    1.0021        0.5633


  Cf ~/blog/logistic_regression-challenger.blog
     ~/webppl/logistic_regression-challenger.wppl

  Compare with logistic_regression.jl for another approach.

=#

using Turing, StatsPlots

include("jl_utils.jl")

@model function logistic_regression_challenger(xs,ys)
    n = length(xs)
    w0 ~ Normal(0, sqrt(1/.001)) # JAGS has dnorm(mu,1/variance)
    w1 ~ Normal(0, sqrt(1/.0001))
    # w0 ~ Normal(0, 10) # JAGS has dnorm(mu,1/variance)
    # w1 ~ Normal(0, 10)

    p = tzeros(n)
    z = tzeros(n)
    for i in 1:n
        z[i] = w0 + w1 * xs[i]
        p[i] = 1 / (1 + exp(-z[i]))
        ys[i] ~ Bernoulli(p[i])
        
        # ys[i] ~ Bernoulli(1 / (1 + exp(-w0 + w1 * xs[i])))
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

model = logistic_regression_challenger(xs,ys)

# chns = sample(model, Prior(), 10_000)
chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))

# display(plot(chns[[:w0,:w1]]) # quite bad!
