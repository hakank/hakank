#=

  From BLOG example/poisson-ball.blog
  """
  Model file for balls in an urn, allowing observation errors. 
  This version uses a Poisson prior for the number of balls.
  """

  Summary Statistics
     parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
         Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

       ...
       numBalls    5.8448    1.9275     0.0193    0.0765    555.1277    1.0003       69.3649
       ...
        numTrue    5.0709    1.5899     0.0159    0.0567    596.1451    1.0043       74.4902
       obsPost1    1.3306    0.4705     0.0047    0.0094   2599.6331    0.9999      324.8323
       obsPost2    1.6406    0.4798     0.0048    0.0090   2731.7459    1.0000      341.3402

  Distributions of variable numBalls (num:0)
  6.00000 =>    1874  (0.187400)
  5.00000 =>    1713  (0.171300)
  7.00000 =>    1510  (0.151000)
  4.00000 =>    1309  (0.130900)
  8.00000 =>    1279  (0.127900)
  9.00000 =>    1012  (0.101200)
  3.00000 =>     921  (0.092100)
  2.00000 =>     348  (0.034800)
  1.00000 =>      34  (0.003400)

  Distributions of variable numTrue (num:0)
  5.00000 =>    2505  (0.250500)
  6.00000 =>    2077  (0.207700)
  4.00000 =>    1928  (0.192800)
  7.00000 =>    1209  (0.120900)
  3.00000 =>    1147  (0.114700)
  8.00000 =>     510  (0.051000)
  2.00000 =>     417  (0.041700)
  9.00000 =>      99  (0.009900)
  1.00000 =>      81  (0.008100)
  10.00000 =>      22  (0.002200)
  0.00000 =>       5  (0.000500)

  Distributions of variable obsPost1 (num:0)
  1.00000 =>    6694  (0.669400)
  2.00000 =>    3306  (0.330600)

  Distributions of variable obsPost2 (num:0)
  2.00000 =>    6406  (0.640600)
  1.00000 =>    3594  (0.359400)
 
  Cf ~/webppl/poisson_Ball.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function poisson_ball(obs=[1,2,1,2,1,2,1,2,1,2])
    n = length(obs)
    Blue = 1
    Green = 2;
    colors = [Blue,Green]

    # True color for each ball
    TrueColor ~ filldist(Categorical([0.5,0.5]),n) # colors

    # Number of balls
    numBalls ~ truncated(Poisson(6),0,n-1)

    # Which balls is drawn
    BallDrawn ~ filldist(DiscreteUniform(1,numBalls),n)

    # We observe Blue,Green,Blue,Green,Blue,Green,Blue,Green,Blue,Green
    for d in 1:n
        trueColor = TrueColor[BallDrawn[d]]
        if trueColor == Blue
            obs[d] ~ Categorical([0.8,0.2])
        else
            obs[d] ~ Categorical([0.2,0.8])
        end
    end

    # Posterior, first 2 balls
    obs_post = tzeros(2)
    for d in 1:2
        trueColor = TrueColor[BallDrawn[d]]
        if trueColor == Blue
            obs_post[d] ~ Categorical([0.8,0.2])
        else
            obs_post[d] ~ Categorical([0.2,0.8])
        end        
    end

    numTrue ~ Dirac(sum([obs[d] == TrueColor[d] ? 1 : 0 for d in 1:n]))

    obsPost1 ~ Dirac(obs_post[1])
    obsPost2 ~ Dirac(obs_post[2])    
    
end


model = poisson_ball()
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))
                    
show_var_dist_pct(chns,:numBalls)
show_var_dist_pct(chns,:numTrue)
show_var_dist_pct(chns,:obsPost1)
show_var_dist_pct(chns,:obsPost2)



