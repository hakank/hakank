#=
  From BLOG example/aircraft-static.blog

  
  Distributions of variable numAircraft (num:0)
  3.00000 =>    3191  (0.319100)
  4.00000 =>    2608  (0.260800)
  2.00000 =>    1726  (0.172600)
  5.00000 =>    1385  (0.138500)
  6.00000 =>     657  (0.065700)
  1.00000 =>     221  (0.022100)
  7.00000 =>     192  (0.019200)
  8.00000 =>      13  (0.001300)
  9.00000 =>       7  (0.000700)

  location[1]
  Summary Stats:
  Length:         10000
  Missing Count:  0
  Mean:           149.209455
  Minimum:        100.031478
  1st Quartile:   123.426126
  Median:         149.309147
  3rd Quartile:   174.100740
  Maximum:        199.992805

  Also see ~/webppl/aircraft_static.wppl 

=#  

using Turing, StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

@model function aircraft_static()
    epsilon = 0.05
    
    # Poisson might get 0 which is not good for this model
    pp ~ Poisson(3.0) 
    numAircraft ~ Dirac(pp == 0 ? 1 : pp)
   
    blipPerAircraft = tzeros(numAircraft)
    location = tzeros(numAircraft)
    blipLocation = tzeros(numAircraft)
    for b in 1:numAircraft
        blipPerAircraft[b] ~ Poisson(1)
        location[b] ~ Uniform(100.0,200.0)
        if blipPerAircraft[b] > 0
            blipLocation[b] ~ Normal(location[b],1.0)
        else
            blipLocation[b] ~ Uniform(90,210)
        end
    end
    
    sumBlips ~ Dirac(sum(blipPerAircraft))

    true ~ Dirac(sumBlips == 4)
    locationB0 ~ Dirac(location[1])
end

model = aircraft_static()

num_chains = 4
# chains = sample(model, Prior(), 1000)
# chains = sample(model, MH(), 100_000)
chains = sample(model, PG(5), 10_000)
# chains = sample(model, IS(), 10_000)
# chains = sample(model, SMC(), 40_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)


display(chains)
# display(plot(chains))

show_var_dist_pct(chains, :numAircraft)

println("\nlocation[1]")
display(summarystats(chains[Symbol("location[1]")]))
