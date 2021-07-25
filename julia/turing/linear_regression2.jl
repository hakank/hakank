#=

  Linear regression

  This is actually a WebPPL example (example/linear_regression.wppl) but translated via my BLOG model.

  xs = [0,1,2,3]
  ys = [0,1,4,6]

  mu should be ~ 2 and b ~ 0.
  y4: y for x=4 should be 2*4+0 = 8
  y5: y for x=5 should be 2*5+0 = 10

  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           m    2.0134    0.3960     0.0040    0.0088   1980.8480    1.0001      445.5349
           b   -0.2678    0.7118     0.0071    0.0135   2785.0095    0.9999      626.4079
       sigma    0.8463    0.4970     0.0050    0.0155    929.0047    0.9999      208.9529
          y4    7.7715    1.5278     0.0153    0.0314   2240.9116    1.0001      504.0287
          y5    9.7916    1.8747     0.0187    0.0401   2060.8303    1.0000      463.5246

  With 5 data points (x=[0,1,2,3,4] and y=[0,1,4,6,8]) with got a much better result:
  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           m    2.0000    0.0001     0.0000    0.0000    684.0973    0.9999      170.6830
           b    0.0000    0.0004     0.0000    0.0000   1140.3205    1.0005      284.5111
       sigma    0.0002    0.0003     0.0000    0.0000    111.5388    1.0486       27.8290
          y4    8.0000    0.0005     0.0000    0.0000    713.2871    1.0046      177.9658
          y5   10.0000    0.0006     0.0000    0.0000    553.0126    1.0016      137.9772


  cf ~/blog/linear_regression2.blog
     ~/webppl/linear_regression2.wppl
=#

using Turing
include("jl_utils.jl")

@model function linear_regression2(xs,ys)
    n = length(xs)
    
    m ~ Normal(0,2)
    b ~ Normal(0,2)
    sigma ~ Gamma(1,1)

    xs ~ filldist(Uniform(0,14),n)
    
    for i in 1:n
        ys[i] ~ Normal(m*xs[i] + b, sigma)
    end

    # Prediction for x=4 and x=5
    y4 ~ Normal(m*4 + b, sigma)
    y5 ~ Normal(m*5 + b, sigma)    

end

xs = [0,1,2,3]
ys = [0,1,4,6]
n = 4
xs = [i for i in 0:n]
ys = [2*x for x in xs]
println("xs: ", xs)
println("ys: ", ys)
model = linear_regression2(xs,ys)

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)
chns = sample(model, NUTS(), 10_000)


display(chns)
