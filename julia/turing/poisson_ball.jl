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
       numBalls    5.8985    1.8454     0.0185    0.0695    677.0798    1.0002      125.6178
   ...
        numTrue    5.0069    1.5379     0.0154    0.0547    816.5438    1.0006      151.4924
       obsPost1    1.3249    0.4684     0.0047    0.0088   2952.0720    1.0000      547.6942
       obsPost2    1.6475    0.4778     0.0048    0.0096   2963.4386    1.0006      549.8031

  Distributions of variable numBalls (num:0)
  5.00000 =>    1996  (0.199600)
  8.00000 =>    1678  (0.167800)
  7.00000 =>    1588  (0.158800)
  6.00000 =>    1548  (0.154800)
  4.00000 =>    1371  (0.137100)
  3.00000 =>     827  (0.082700)
  9.00000 =>     752  (0.075200)
  2.00000 =>     204  (0.020400)
  1.00000 =>      36  (0.003600)

  Distributions of variable numTrue (num:0)
  5.00000 =>    2486  (0.248600)
  4.00000 =>    2180  (0.218000)
  6.00000 =>    2040  (0.204000)
  7.00000 =>    1216  (0.121600)
  3.00000 =>    1205  (0.120500)
  8.00000 =>     368  (0.036800)
  2.00000 =>     261  (0.026100)
  1.00000 =>     130  (0.013000)
  9.00000 =>      94  (0.009400)
  10.00000 =>      11  (0.001100)
  0.00000 =>       9  (0.000900)

  Distributions of variable obsPost1
  blue       =>    6751  (0.675100)
  green      =>    3249  (0.324900)

  Distributions of variable obsPost2
  green      =>    6475  (0.647500)
  blue       =>    3525  (0.352500)
 
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
show_var_dist_pct(chns,:obsPost1,["blue","green"])
show_var_dist_pct(chns,:obsPost2,["blue","green"])



