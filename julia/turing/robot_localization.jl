#=
    This is a port of the SPPL model robot-localization.pynb
    The example just shows the model.

    For this Turing model we observe that x is in 1..3

 Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

       param   24.5960   14.4996     0.4585    1.4675   119.6569    0.9995       86.5824
       which    0.4624    0.2851     0.0090    0.0259    84.4402    1.0284       61.1000
           x    2.0981    0.5983     0.0189    0.0636   131.0042    1.0141       94.7932
   


=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function robot_localization() 
    param ~ DiscreteUniform(0, 101)
    which ~ Uniform(0, 1)
    if which < 0.9
        x ~ Normal(param/10, 1)
    elseif which < 0.95
        x ~ Uniform(0, 10)
    else
        x ~ Dirac(10.0)
    end

    # Some observation
    true ~ Dirac( 1.0 < x < 3.0)
end 

model = robot_localization()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
