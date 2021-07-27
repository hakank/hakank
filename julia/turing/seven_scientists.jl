#=
  Seven scientists in Turing.jl

  Port of the WebPPL model from
  https://mhtess.github.io/bdappl/chapters/03-simpleModels.html
  """
  Seven scientists with wildly-differing experimental skills all make a 
  measurement of the same quantity. They get the answers 

     x = {−27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056}. 

  Intuitively, it seems clear that the first two scientists are pretty inept 
  measurers, and that the true value of the quantity is probably just a bit 
  below 10. The main problem is to find the posterior distribution over 
  the measured quantity, telling us what we can infer from the measurement. 
  A secondary problem is to infer something about the measurement skills of 
  the seven scientists.

  """

  Using NUTS(1000,0.65):
  """
  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

          mu    9.1691    1.5779     0.0158    0.0388   1514.9796    1.0003      361.9158
   sigmas[1]   17.1826    2.1492     0.0215    0.0481   1812.3976    1.0024      432.9665
   sigmas[2]   10.1038    4.8808     0.0488    0.0947   2207.2242    1.0000      527.2872
   sigmas[3]    6.9711    5.3999     0.0540    0.1307   1840.5852    1.0000      439.7002
   sigmas[4]    5.7966    5.5111     0.0551    0.1452   1247.6705    1.0002      298.0579
   sigmas[5]    5.5648    5.4101     0.0541    0.1455   1266.5753    1.0008      302.5741
   sigmas[6]    5.6467    5.3356     0.0534    0.1272   1428.5790    1.0002      341.2754
   sigmas[7]    5.8013    5.4090     0.0541    0.1533   1491.1203    1.0003      356.2160

  Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

          mu    4.4711    8.7121    9.6185    9.9814   11.3731
   sigmas[1]   12.1421   15.8454   17.5997   18.9317   19.8960
   sigmas[2]    2.3154    6.0300    9.5638   14.0701   19.2536
   sigmas[3]    0.4744    2.4507    5.4174   10.7118   18.7471
   sigmas[4]    0.1709    1.2017    3.7592    9.4112   18.4969
   sigmas[5]    0.1470    1.1639    3.5180    8.8308   18.5041
   sigmas[6]    0.1892    1.3070    3.7946    8.5708   18.6025
   sigmas[7]    0.1556    1.2915    3.7926    9.2773   18.1375
   """

=#

using Turing
include("jl_utils.jl")

@model function seven_scientists(data)
    n = length(data)
    mu ~ Normal(0, 30)
    sigmas ~ filldist(Uniform(0,20),n)
    for i in 1:n
        data[i] ~ Normal(mu, sigmas[i])
    end
end
data = [-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056]

model = seven_scientists(data)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, IS(), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, SMC(), MCMCThreads(), 10_000, 4)

chns = sample(model, NUTS(1000,0.65), 10_000)
# chns = sample(model, HMC(0.1,10), 10_000)

display(chns)

