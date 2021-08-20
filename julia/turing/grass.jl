#=

    This is a port of the R2 model Grass.cs

    We observe that the grass is wet. Did it rain?

    Output from the R2 model:
    ```
    Mean: 0.699
    Variance: 0.21061
    Number of accepted samples = 790
    ```

    This model:
    
Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

      cloudy    0.5530    0.4974     0.0157    0.0225   518.9849    0.9997       63.1061
        rain    0.6890    0.4631     0.0146    0.0215   439.4618    1.0015       53.4365
   sprinkler    0.4240    0.4944     0.0156    0.0196   422.0078    1.0014       51.3142
       temp1    0.7100    0.4540     0.0144    0.0197   523.3179    0.9990       63.6330
     wetRoof    0.4850    0.5000     0.0158    0.0231   457.4633    0.9992       55.6254
       temp2    0.9690    0.1734     0.0055    0.0107   514.7809    1.0017       62.5950
       temp3    0.9280    0.2586     0.0082    0.0125   565.5121    0.9990       68.7636
    wetGrass    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

# Did it rain?
@model function grass() 
    cloudy ~ flip(0.5)

    if cloudy
        rain ~ flip(0.8)
        sprinkler ~ flip(0.1)
    else
        rain ~ flip(0.2)
        sprinkler ~ flip(0.5)
    end

    temp1 ~ flip(0.7)

    wetRoof ~ Dirac(temp1 && rain)

    temp2 ~ flip(0.9)
    temp3 ~ flip(0.9)

    wetGrass ~ Dirac(temp2 && rain || temp3 && sprinkler)

    # We observe that the grass is wet.
    # Did it rain?
    true ~ Dirac(wetGrass)

end 

model = grass()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)


display(chns)


