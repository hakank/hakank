#=

  From srl-pp-tutorial-wasp-stockholm.pdf
  "Statistical Relational learning and Probabilistic Programming"
  by Luc De Raedt, Anton Dries, and Angelika Kimmig
  https://dtai.cs.kuleuven.be/problog/wasp17-tutorial.html
  
  slide 394f:
  """
  - Flipping a coin with unknown weight
  - Prior: uniform distribution on [0,1]
  - Observation: 5x heads in a row
  """

  The ProbLog model return the following which corresponds to the
  density of the probabilist 
  """
   weight(c1,0.1): 3.3994697e-13
   weight(c1,0.3): 2.1679411e-06
   weight(c1,0.5): 0.0041497433
   weight(c1,0.7): 0.1317485 
   weight(c1,0.9): 0.86409959         <----
   weight(c2,0.1): 3.2276726e-06
   weight(c2,0.3): 0.024109997
   weight(c2,0.5): 0.66724754         <----
   weight(c2,0.7): 0.30628626
   weight(c2,0.9): 0.0023529733
  """

  Here we observe 13 tosses and detecting the probability of throwing a head.

  Cf ~/problog/coins_learning.pl
     ~/blog/coins_learning.blog
     ~/webppl/coins_learning.wppl

  Note that this model is much simpler than the ProbLog model since ProbLog 
  don't support continous distributions. 
  Which makes it clear that I might have missed the point of the ProbLog model. :-)
  
  For a model which is much more close to the ProbLog model, see coins_learning2.jl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function coins_learning(obs)
    n = length(obs)
    p ~ Uniform(0,1)
    obs ~ filldist(flip(p),n)
    # post ~ filldist(flip(p),n)
    
end

# p: 0.9344
# obs = [true for _ in 1:13]
# p: 0.5984
obs = [true,false,true,true,true,true,true,false,false,true,false,false,true]
model = coins_learning(obs)
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(1000), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

