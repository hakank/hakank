#=
  Cookie problem.

  From Think Bayes, page 3
  """
  Suppose there are two bowls of cookies. 
  Bowl 1 contains 30 vanilla cookies and 10 chocolate cookies. 
  Bowl 2 contains 20 of each.

  Now suppose you choose one of the bowls at random and, without looking,
  select a cookie at random. The cookie is vanilla. 
  What is the probability that it came from Bowl 1?
  """

  Distributions of variable bowl (num:0)
  1.00000 =>    5991  (0.599100)
  2.00000 =>    4009  (0.400900)

  Cf  ~/webppl/cookie_problem.wppl


=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function cookie_problem()
    vanilla = 1
    chocolate = 2
    
    bowl1 = 1
    bowl2 = 2
    
    bowl ~ DiscreteUniform(bowl1,bowl2)
    cookie ~ (bowl == bowl1) ? Categorical(simplex([30,10])) : # [vanilla,chocolate] 
                               Categorical(simplex([20,20]))
    
    true ~ Dirac(cookie == vanilla)

end

model = cookie_problem()
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))


show_var_dist_pct(chns, :bowl)
