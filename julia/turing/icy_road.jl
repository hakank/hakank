#=
   From Hugin's icy_roads.net
   """
   This example shows d-separation in action. The risks of Holmes and Watson crashing are only dependent 
   the state of icy is not known.
 
   If it is known, that the roads are icy, they both have a large risk of crashing. Likewise, if the 
   roads are not icy they both have a small risk of crashing.

   But if the state of icy is not known, and Holmes crashes, the risk of Watson crashing goes up, since 
   the crash of Holmes indicates that the roads may be icy.
   """

  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

         icy    0.9465    0.2250     0.0023    0.0030   4799.0536    1.0003      714.6766
      watson    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
      holmes    0.7622    0.4258     0.0043    0.0055   4960.2429    1.0005      738.6810


   cf ~/blog/icy_road.blog
      ~/psi/icy_road.psi
      ~/webppl/icy_road.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function icy_road()
    icy ~ flip(0.7)
    
    # This node gives the risk of Watson crashing in his car, given the state of the roads.
    watson ~ icy ? flip(0.8) : flip(0.1)
    
    # This node gives the risk of Holmes crashing in his car, given the state of the roads.
    holmes ~ icy ? flip(0.8) : flip(0.1)
        
    # true ~ Dirac(icy == true)
    true ~ Dirac(watson == true)
    # true ~ Dirac(watson == false)
    


end

model = icy_road()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

