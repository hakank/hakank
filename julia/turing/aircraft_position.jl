#=
  Aircrat position.
  From BLOG example/aircraft-position.blog

  Distributions of variable numAircraft (num:0)
  3.00000 =>    5857  (0.585700)
  4.00000 =>    3178  (0.317800)
  5.00000 =>     833  (0.083300)
  6.00000 =>     114  (0.011400)
  7.00000 =>      18  (0.001800)

  Also see ~/webppl/aircraft_position.wppl 
=#  

using Turing, StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

@model function aircraft_position()
    epsilon = 0.05
    
    # Poisson might get 0 which is not good for this model
    pp ~ Poisson(5) 
    numAircraft ~ Dirac(pp == 0 ? 1 : pp)

    blips = tzeros(numAircraft)
    positions = tzeros(numAircraft)
    obsPos = tzeros(numAircraft)
    for b in 1:numAircraft
        blips[b] ~ flip(0.9)
        positions[b] ~ Normal(0,10)
        obsPos[b] ~ Normal(positions[b],1)
    end
    
    sumBlips ~ Dirac(sum(blips))

    function inRange(x,y,epsilon)
        Dirac(x > y - epsilon && x < y + epsilon)
    end
    
    true ~ Dirac(sumBlips == 3)
   
    inRange(obsPos[1],5.0,epsilon)
    numAircraftPositionLt5 ~ Dirac(sum([positions[a] > 5 ? 1 : 0 for a in 1:numAircraft]))
end

model = aircraft_position()

num_chains = 4
# chains = sample(model, Prior(), 1000)
# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(5), 10_000)
# chains = sample(model, IS(), 10_000)
# chains = sample(model, SMC(), 10_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)


display(chains)
# display(plot(chains))

show_var_dist_pct(chains, :numAircraft)
