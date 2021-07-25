#=
   BLOG example/uniform-ball.blog
   """
   Model file for balls in an urn, allowing observation errors. 
   This version uses a Poisson prior for the number of balls.
   """


   Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

      numBalls    2.8192    1.0196     0.0102    0.0225   1557.7583    0.9999      464.5864
      ...
             q    0.3318    0.4709     0.0047    0.0114   1815.4962    1.0000      541.4543
      obsPost1    0.6332    0.4820     0.0048    0.0074   5313.9656    1.0000     1584.8391


   Distributions of variable numBalls (num:0)
   4.00000 =>    3325  (0.332500)
   2.00000 =>    2869  (0.286900)
   3.00000 =>    2674  (0.267400)
   1.00000 =>    1132  (0.113200)

  Cf ~/webppl/uniform_ball.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function uniform_ball(obs=[1,0,1,0,1,0,1,0])
    n = length(obs)
    
    numBalls ~ DiscreteUniform(1,4)
    
    isBlue ~ filldist(flip(0.5),n)
    
    ballDrawn ~ filldist(DiscreteUniform(1,numBalls),n)

    for d in 1:n
        if isBlue[ballDrawn[d]]
            obs[d] ~ flip(0.8)
        else
            obs[d] ~ flip(0.2)
        end
    end
    
    q ~ Dirac(ballDrawn[1] == ballDrawn[2])

    
    if isBlue[ballDrawn[1]]
        obsPost1 ~ flip(0.8)
    else
        obsPost1 ~ flip(0.2)
    end
        
 
end

model = uniform_ball()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:numBalls)

