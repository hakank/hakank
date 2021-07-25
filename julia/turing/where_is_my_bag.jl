#=
  Where is my bag problem.

  From
  "Probabilistic Reasoning Under Uncertainty with Bayesian Networks"
  https://www.bayesia.com/2018-03-02-probabilistic-reasoning-under-uncertainty

  (Also see The BaysiaLabBook "Bayesian Networks BayesiaLab" v1, page 51f.)

  Travel from Singapore -> Tokyo -> Los Angeles.
  1) The departure of Singapore -> Tokyo is delayed
  2) Just made the Tokyo -> LA trip. There's a 50/50 change the luggage is on the plane to LA.
  3) Arriving at LA, waiting for the luggage in 5 minutes, the luggage has not been seen.
  What is the probability that the bag was in the Tokyo -> LA plane?
  
  Answer: The probability that the bag was with the LA plane is 33% (as in the talk).

  From Judea Pearl: The Book of Why.

  For p(bag_on_plane) = 0.5
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
  bag_on_plane    0.3364    0.4725     0.0047    0.0053   8953.8759    1.0001     1581.1188


  For p(bag_on_plane) = 0.8
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
  bag_on_plane    0.6641    0.4723     0.0047    0.0051   8363.8461    0.9999     1469.6619


  Cf ~/blog/where_is_my_bag.blog
     ~/webppl/where_is_my_bag.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function where_is_my_bag(maxTime=10,p=0.5,time=5.0,bag_on_carousel=false)    

    # We know that the bag was on the plane with 50% probability.
    # Variant: https://www.youtube.com/watch?v=c71pnonOMkI&t=1074s it's 80/20.
    bag_on_plane ~ flip(p)
    
    # We assume uniform time step
    time ~ DiscreteUniform(1,maxTime)
    
    # Probability that the bag is on the carousel given a specific time
    # (if it was on the plane, that is).
    # The probability that the bag is on the carousel on a given time
    # is directly proportional on the time:
    #     time / maxTime
    bag_on_carousel ~ bag_on_plane == true ? flip(1.0*time/maxTime) : Dirac(0.0)
   
    # He waited 5 minutes without seeing the luggage.
    # Note: It's faster if we observe via the parameters
    # true ~ Dirac(time == 5.0)
    # true ~ Dirac(bag_on_carousel == false)

 
end

maxTime = 10
p = 0.5
# p = 0.8
time = 5.0
bag_on_carousel = false
model = where_is_my_bag(maxTime,p,time,bag_on_carousel)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

