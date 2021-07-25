#=

  Repeated measures of IQ in Turing.jl

  From WebPPL model
  https://mhtess.github.io/bdappl/chapters/03-simpleModels.html
  """
  The data are the measures xijx_{ij}xij​ 
     for the i=1,...,ni = 1, . . . ,ni=1,...,n people 
  and their j=1,...,mj = 1, . . . ,mj=1,...,m repeated test scores.

  We assume that the differences in repeated test scores are distributed as 
  Gaussian error terms with zero mean and unknown precision. The mean of the 
  Gaussian of a person’s test scores corresponds to their latent true IQ. 
  This will be different for each person. The standard deviation of the 
  Gaussians corresponds to the accuracy of the testing instruments in 
  measuring the one underlying IQ value. We assume this is the same for 
  every person, since it is conceived as a property of the tests themselves.
  """

  Note: The WebPPL model has sigma ~ Uniform(0,50) and get these values, with
  a quite low sigma (~ 7)
   """
   sigma: 6.996331408573459
   p1:94.81734864858849
   p2:109.78959481757205
   p3:154.67515393166423

   credibleInterval on sigma:
   [ '0.90', [ 3.3645266702554633, 10.402949581214315 ] ]
   credibleInterval on p1:
   [ '0.90', [ 88.48060304863435, 102.16143539836233 ] ]

   credibleInterval on p2:
   [ '0.90', [ 102.73276331774267, 116.84164858686201 ] ]

   credibleInterval on p3:
   [ '0.90', [ 149.12746700987051, 161.4365469824775 ] ]
   """

  This Turing.jl model with sigma ~ Uniform(0,50) give a much larger posterior sigma (~20)
  """
  parameters       mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
       sigma    19.9092   13.8640     0.1386    1.1993    26.2159    1.8145       54.6165
      mus[1]    89.2099   25.0480     0.2505    0.7799   881.3664    1.0013     1836.1801
      mus[2]   105.8519   23.4151     0.2342    0.9114   533.6730    1.0020     1111.8188
      mus[3]   146.5696   22.5392     0.2254    0.8805   442.2614    1.0029      921.3778
  ...
  Credible interval for sigma with mass 0.9: (7.636206..42.867467)

  """

  So I lower the interval range of sigma and get the following:
  """
  Summary Statistics
  parameters       mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
      Symbol    Float64   Float64    Float64   Float64   Float64   Float64       Float64 

       sigma     6.0077    2.9381     0.0294    0.2733   29.9863    1.0457       13.0205
      mus[1]    91.2915    6.6268     0.0663    0.5185   63.0714    1.1298       27.3866
      mus[2]   103.7605    6.4221     0.0642    0.4227   89.3549    1.0412       38.7993
      mus[3]   148.7040    6.3352     0.0634    0.4894   98.1541    1.0009       42.6201

  Quantiles
  parameters       2.5%      25.0%      50.0%      75.0%      97.5% 
      Symbol    Float64    Float64    Float64    Float64    Float64 

       sigma     2.8435     3.4661     4.1825     8.8713    11.3996
      mus[1]    76.6420    89.0146    92.6321    94.7485   105.7608
      mus[2]    91.8652   100.3283   104.2820   106.5811   119.8080
      mus[3]   137.4726   146.9825   148.9408   148.9408   162.6477

  Credible interval for sigma with mass 0.9: (3.466133..11.095075)
  Credible interval for mus[1] with mass 0.9: (78.278793..97.360210)
  Credible interval for mus[2] with mass 0.9: (91.267542..109.106586)
  Credible interval for mus[3] with mass 0.9: (140.436663..160.814064)
  """

  And it's still larger credible intervals...

=#
using Turing
include("jl_utils.jl")

@model function repeated_iq_measurements(data)
    n,_ = size(data)
    
    # everyone shares same sigma (corresponding to measurement error)
    sigma ~ Uniform(0, 12)

    # each person has a separate latent IQ
    mus ~ filldist(Uniform(0,200),n)
    for i in 1:n
        for j in 1:length(data[i])
            data[i,j] ~ Normal(mus[i], sigma)
        end
    end

end

data = [
   90 95 100
   105 110 115
   150 155 160]
println("data:", data) 
model = repeated_iq_measurements(data)

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, NUTS(), 10_000)
# chains = sample(model, HMC(0.1,10), 10_000)

display(chains)

credible_interval(chains,"sigma",0.90)
credible_interval(chains,"mus[1]",0.90)
credible_interval(chains,"mus[2]",0.90)
credible_interval(chains,"mus[3]",0.90)
