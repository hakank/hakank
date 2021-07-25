#=
  From a BLOG model in
  Yi Wu, Lei Li, Stuart Russell, Rastislav Bodik
  "Swift: Compiled Inference for Probabilistic Programming Languages"  
  Page 2
  """
  [The model] demonstrates the open-universe urn-ball model. In this version
  the query asks for the color of the next random pick from an urn given the
  colors of balls drawn previously.
  """

  Summary Statistics
  parameters      mean       std   naive_se      mcse        ess       rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64   Float64?   Float64?      Float64? 

    numBalls    2.6863    3.6429     0.0364    0.2942   107.7806     1.0000        4.7524
    ...
     drawnD5    1.7703    1.8844     0.0188    0.1436   120.3713     1.0076        5.3076
     colorD5    1.8228    0.3819     0.0038    0.0296   107.6107     0.9999        4.7450


   Distributions of variable numBalls (num:0)
   1.00000 =>    5969  (0.596900)
   2.00000 =>    1409  (0.140900)
   3.00000 =>     843  (0.084300)
   4.00000 =>     489  (0.048900)
   9.00000 =>     319  (0.031900)
   5.00000 =>     242  (0.024200)
   19.00000 =>     209  (0.020900)
   7.00000 =>     161  (0.016100)
   6.00000 =>     109  (0.010900)
   15.00000 =>      72  (0.007200)
   16.00000 =>      54  (0.005400)
   20.00000 =>      46  (0.004600)
   11.00000 =>      42  (0.004200)
   12.00000 =>      36  (0.003600)

   Distributions of variable drawnD5 (num:0)
   1.00000 =>    7070  (0.707000)
   2.00000 =>    1149  (0.114900)
   3.00000 =>     905  (0.090500)
   4.00000 =>     449  (0.044900)
   10.00000 =>     109  (0.010900)
   9.00000 =>      72  (0.007200)
   5.00000 =>      68  (0.006800)
   8.00000 =>      54  (0.005400)
   6.00000 =>      42  (0.004200)
   19.00000 =>      38  (0.003800)
   7.00000 =>      36  (0.003600)
   11.00000 =>       8  (0.000800)

   Distributions of variable colorD5
   blue       =>    8228  (0.822800)
   green      =>    1772  (0.177200)

   I.e. it's quite probable that there is just one ball (or perhaps two).

  Cf ~/blog/urn_ball.blog
     ~/psi/urn_ball.psi
     ~/webppl/urn_ball.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

# Blue: 1 Green: 2
# We observe 4 Green balls
@model function urn_ball(obs=[2,2,2,2])
    n = 5
    
    blue = 1
    green = 2
   
    numBalls ~ DiscreteUniform(1,20)

    # PG(5) throws
    # ERROR: LoadError: CTaskException:
    # DimensionMismatch("tried to assign 7 elements to 16 destinations")
    # for this filldist() expression
    # color ~ filldist(Categorical([0.9,0.1]),numBalls) # [Blue,Green]

    # PG(5) don't have any problems with this, though
    color = tzeros(numBalls)
    for i in 1:numBalls
        color[i] ~ Categorical([0.9,0.1]) # [Blue,Green]
    end

    drawn ~ filldist(DiscreteUniform(1,numBalls),n)
    
    for i in 1:length(obs)
        obs[i] ~ Dirac(color[drawn[i]])
    end

    drawnD5 ~ Dirac(drawn[5])
    colorD5 ~ Dirac(color[drawn[5]])


end

obs=[2,2,2,2] # Observed only green balls
model = urn_ball(obs)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:numBalls)
show_var_dist_pct(chns,:drawnD5)
show_var_dist_pct(chns,:colorD5,["green","blue"])

