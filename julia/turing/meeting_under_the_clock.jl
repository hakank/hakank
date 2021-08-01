#=
   Meeting Under the clock (Julian Simon 1994)
   
   """
   Meeting Under the Clock (This problem is posed by Julian Simon(1994))

   Two persons agree to arrive at the two clock sometime between 1 pm and 2 pm 
   and to stay for 20 minutes. What is the probability that they will be there
   at the same time?
   """

   Summary Statistics
parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

          c1   30.1994   17.2942     0.1729    0.1723    9331.2798    0.9999     7267.3518
          c2   30.1059   17.3087     0.1731    0.1659   10124.1004    1.0000     7884.8134
        prob    0.5734    0.4946     0.0049    0.0054    9476.5095    1.0000     7380.4591

    According to the WebPPL model meeting_under_the_clock.wppl 
    the exact probability is 0.5742602700373461.


    Cf ~/webppl/meeting_under_the_clock.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function meeting_under_the_clock(w=20)
    c1 ~ DiscreteUniform(1,60)
    c2 ~ DiscreteUniform(1,60)
    # meeting within waiting time
    prob ~ Dirac(abs(c1-c2) <= w)
end

w = 20
model = meeting_under_the_clock(w)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 1_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))
