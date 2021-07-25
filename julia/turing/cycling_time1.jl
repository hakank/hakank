#=
  CyclingTime1 Example from
  "Infer.NET 101 A sample-based introduction to the basics of 
  Microsoft Infer.NET programming", page 12ff.
  """
  averageTimePosterior: Gaussian(15.29, 1.559)
  trafficNoisePosterior: Gamma(1.458, 0.3944)[mean=0.5751]
  ...
  Tomorrows predicted time: 15.29 plus or minus 2.66
  ...
  Probability that the trip takes less than 18 min: 0.85
  """

  Summary Statistics
                   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                       Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

                  averageTime   15.3182    1.0817     0.0108    0.0357    816.4845    0.9999      209.3550
                 trafficNoise    1.8035    0.5902     0.0059    0.0223    761.6531    1.0011      195.2957
                tomorrowsTime   15.3090    2.1760     0.0218    0.0398   2891.2802    1.0000      741.3539
  probTripLongerThan18Minutes    0.9088    0.2879     0.0029    0.0046   4217.2755    0.9999     1081.3527

  Cf ~/webppl/cycling_time1.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function cycling_time1(travelTimes=[13,17,16])
    averageTime ~ Normal(15,sqrt(100.0))
    trafficNoise ~ Gamma(2.0,1/2.0)
    
    # travelTimeMonday    ~ Normal(averageTime,trafficNoise)
    # travelTimeTuesday   ~ Normal(averageTime,trafficNoise)
    # travelTimeWednesday ~ Normal(averageTime,trafficNoise)
    # Better:
    travelTimes ~ filldist(Normal(averageTime,trafficNoise),3)
    tomorrowsTime       ~ Normal(averageTime,trafficNoise)
    
    probTripLongerThan18Minutes ~ Dirac(tomorrowsTime < 18.0)
        
end

model = cycling_time1()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
# display(plot(chns))
