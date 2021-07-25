#=

  From BLOG example/csi:
  """
  Model that illustrates context-specific independence.  This is a 
  parameterization of the model shown in Fig. 3 of [Milch et al., 
  AISTATS 2005]: X depends on W when U is true, but on V when U is false.  
  
  The probability of X being true given no evidence is P(X=true) = 0.596.
  """

  Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

           u    0.2964    0.4567     0.0046    0.0045    9896.4994    0.9999     2267.2393
           v    0.9005    0.2993     0.0030    0.0030    9747.9184    0.9999     2233.2001
           w    0.0971    0.2961     0.0030    0.0034   10124.1395    0.9999     2319.3905
           x    0.5931    0.4913     0.0049    0.0049    9952.7108    0.9999     2280.1170


  Cf ~/webppl/csi.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function csi()
    u ~ flip(0.3)
    v ~ flip(0.9)
    w ~ flip(0.1)
    
    x ~
        (u) ? (w == true ? flip(0.8) : flip(0.2)) :
        (v) ? flip(0.8) : flip(0.2)
   
end

model = csi()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
# display(plot(chns))
