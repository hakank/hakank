#=
  M & M problem.

  From Think Bayes, page 6f.
  """
  M&M’s are small candy-coated chocolates that come in a variety of colors.
  Mars, Inc., which makes M&M’s, changes the mixture of colors from time
  to time.

  In 1995, they introduced blue M&M’s. Before then, the color mix in a bag
  of plain M&M’s was 30% Brown, 20% Yellow, 20% Red, 10% Green, 10%
  Orange, 10% Tan. Afterward it was 24% Blue , 20% Green, 16% Orange,
  14% Yellow, 13% Red, 13% Brown.

  Suppose a friend of mine has two bags of M&M’s, and he tells me that one
  is from 1994 and one from 1996. He won’t tell me which is which, but he
  gives me one M&M from each bag. One is yellow and one is green. What is
  the probability that the yellow one came from the 1994 bag?

  """

  Page 7 (the table): The answer is 20/27: ~0.74074.


   Distributions of variable mix0
   mix1994    =>    7442  (0.744200)
   mix1996    =>    2558  (0.255800)

   Distributions of variable mix1
   mix1996    =>    7442  (0.744200)
   mix1994    =>    2558  (0.255800)

   Distributions of variable color0
   yellow     =>   10000  (1.000000)

   Distributions of variable color1
   green      =>   10000  (1.000000)

  Cf ~/webppl/m_and_m_problem.wppl

=#

using Turing, StatsPlots

include("jl_utils.jl")

@model function m_and_m_problem()
    brown  = 1
    yellow = 2
    red    = 3
    green  = 4
    orange = 5
    tan    = 6
    blue   = 7
    colors = [brown,yellow,red,green,orange,tan,blue]
    
    mix1994 = 1
    mix1996 = 2
    mixes = [mix1994, mix1996]
    
    # First pick a bag in mix0 and then pick the other bag in mix1
    mix0 ~ UniformDraw(mixes)
    mix1 ~ Dirac(mix0 === mix1994 ? mix1996 : mix1994)

    # The mixes of each colors for the two years
    mix1994ps = [30,20,20,10,10,10,0]
    mix1996ps = [13,14,13,20,16,0,24]
    color0 ~ mix0 == mix1994 ? DiscreteNonParametric(colors,simplex(mix1994ps)) :
                               DiscreteNonParametric(colors,simplex(mix1996ps))

    color1 ~ mix1 == mix1994 ? DiscreteNonParametric(colors,simplex(mix1994ps)) :
                               DiscreteNonParametric(colors,simplex(mix1996ps))
       
    true ~ Dirac(color0 == yellow)
    true ~ Dirac(color1 == green)


end

model = m_and_m_problem()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:mix0,[:mix1994, :mix1996])
show_var_dist_pct(chns,:mix1,[:mix1994, :mix1996])

println()
show_var_dist_pct(chns,:color0,["brown","yellow","red","green","orange","tan","blue"])
show_var_dist_pct(chns,:color1,["brown","yellow","red","green","orange","tan","blue"])
