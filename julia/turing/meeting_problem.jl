#=
   Meeting problem.
   
   (I'm not sure about the source. The page I got it from does not exist anymore.)
   """
   Let's say that 4 people agree to meet between 3:00 P.M. and 4:00 P.M.. 
   One man can wait for 10 minutes. Another can wait for 15 minutes. 
   Another can wait for 20 Minutes, and another still can wait for 30 
   minutes. What is the probability that all 4 will meet?
   """

   Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        prob    0.0836    0.2768     0.0028    0.0030   9775.6911    0.9999     7783.1935


    Cf meeting_under_the_clock,jl 

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function meeting_problem(w=[10,15,20,30])
    n = length(w)
    ps = tzeros(n)
    for i in 1:n 
        ps[i] ~ DiscreteUniform(1,60)
    end
    # Will all people meet?
    prob ~ Dirac(reduce(intersect, [Set(ps[i]:ps[i]+w[i])  for i in 1:n]) != Set())
end

w = [10,15,20,30]
model = meeting_problem(w)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 1_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns[[:prob]])
# display(plot(chns))
