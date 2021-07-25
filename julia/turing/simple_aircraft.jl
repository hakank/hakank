#=

   From BLOG example/simple-aircraft.blog

   Summary Statistics
   parameters      mean       std   naive_se      mcse        ess       rhat   ess_per_sec 
       Symbol   Float64   Float64    Float64   Float64   Float64?   Float64?      Float64? 

  numAircraft    1.4000    0.5831     0.0058    0.0519    57.7156     1.0166       18.9791
   numBlip[1]    2.4750    0.8212     0.0082    0.0722    61.7070     1.0233       20.2917
     numBlip1    2.4750    0.8212     0.0082    0.0722    61.7070     1.0233       20.2917
   numBlip[2]    1.3214    0.6577     0.0111    0.1033    missing    missing       missing
   numBlip[3]    1.2500    0.4334     0.0194    0.1936    missing    missing       missing

  Distributions of variable numAircraft (num:0)
  1.00000 =>    6500  (0.650000)
  2.00000 =>    3000  (0.300000)
  3.00000 =>     500  (0.050000)

  Distributions of variable numBlip1 (num:0)
  3.00000 =>    6750  (0.675000)
  1.00000 =>    1750  (0.175000)
  2.00000 =>    1375  (0.137500)
  0.00000 =>     125  (0.012500)

  Cf ~/webppl/simple_aircraft.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function simple_aircraft(sumBlip=3)
    numAircraft ~ Poisson(5)

    # each Aircraft has a blip
    numBlip ~ filldist(Poisson(4), numAircraft)

    # We observe that there are in all 3 blips.
    sumBlip ~ Dirac(sum(numBlip))

    numBlip1 ~ Dirac(numBlip[1])
   
end

model = simple_aircraft()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :numAircraft)
show_var_dist_pct(chns, :sumBlip)
show_var_dist_pct(chns, :numBlip1)
