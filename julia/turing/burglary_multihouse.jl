#=

  From BLOG example/burglary-multihouse.blog

  Summary Statistics
   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
       Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

  burglary[1]    0.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
  burglary[2]    0.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
  burglary[3]    0.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
  burglary[4]    0.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
   earthquake    0.9208    0.2701     0.0027    0.0270     23.2091    1.0899        0.5575
     alarm[1]    0.9994    0.0245     0.0002    0.0006   1543.6492    1.0005       37.0775
     alarm[2]    0.9992    0.0283     0.0003    0.0008   1153.7201    1.0007       27.7117
     alarm[3]    0.9992    0.0283     0.0003    0.0008   1153.7201    1.0007       27.7117
     alarm[4]    0.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN


  Cf ~/webppl/burglary_multihouse.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function burglary_multihouse(evidence=[true,true,true,false])
    mary = 1
    john = 2
    cathy = 3
    roger = 4
    n = 4
    
    burglary ~ filldist(flip(0.003),n)
    earthquake ~ flip(0.002)
    alarm = tzeros(n)
    for p in 1:n
        alarm[p] ~ (burglary[p] == false && earthquake ==false)  ? flip(0.01) : 
                   (burglary[p] == false && earthquake == true)  ? flip(0.40) :
                   (burglary[p] == true  && earthquake == false) ? flip(0.80) :
                   (burglary[p] == true  && earthquake == true ) ? flip(0.90) : flip(0.0)
    end

    # for p in 1:n
    #     true ~ Dirac(alarm[p] == evidence[p])
    # end
    true ~ Dirac(alarm[mary] == true)
    true ~ Dirac(alarm[john] == true)
    true ~ Dirac(alarm[cathy] == true)
    true ~ Dirac(alarm[roger] == false)            
end

# alarms from [mary,john,cathy,roger]
evidence=[true,true,true,false]
model = burglary_multihouse(evidence)

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(10_000), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,10), 10_000)

display(chns)
# display(plot(chns))
