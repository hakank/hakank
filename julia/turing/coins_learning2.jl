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

  In this version we simulate the ProbLog model more closely
  than in coins_learning.jl

  Result:
  Distributions of variable param (num:0)
  0.90000 =>    8646  (0.864600)
  0.70000 =>    1313  (0.131300)
  0.50000 =>      41  (0.004100)

  Distributions of variable ix (num:0)
  5.00000 =>    8646  (0.864600)
  4.00000 =>    1313  (0.131300)
  3.00000 =>      41  (0.004100)

  Distributions of variable ix1 (num:0)
  0.00000 =>   10000  (1.000000)

  Distributions of variable ix2 (num:0)
  0.00000 =>   10000  (1.000000)

  Distributions of variable ix3 (num:0)
  0.00000 =>    9959  (0.995900)
  1.00000 =>      41  (0.004100)

  Distributions of variable ix4 (num:0)
  0.00000 =>    8687  (0.868700)
  1.00000 =>    1313  (0.131300)

  Distributions of variable ix5 (num:0)
  1.00000 =>    8646  (0.864600)
  0.00000 =>    1354  (0.135400)



  Cf ~/webppl/coins_learning2.wppl


=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function coins_learning(obs)
    params = [0.1,0.3,0.5,0.7,0.9];    
    params_prior = [0.05,0.2,0.5,0.2,0.05];
    len = length(params)

    #  Generate the index for param/weight interval
    #  according to the priors in params_prior
    ix ~ Categorical(simplex(params_prior)) # [0,1,2,3,4]
    
    #  generate the param value to use for this data point
    obs ~ filldist(flip(params[ix]),length(obs))

    param ~ Dirac(params[ix])
    ix1 ~ Dirac(ix == 1)
    ix2 ~ Dirac(ix == 2)
    ix3 ~ Dirac(ix == 3)
    ix4 ~ Dirac(ix == 4)
    ix5 ~ Dirac(ix == 5)
end

# p: 0.9344
obs = [true for _ in 1:13]
# p: 0.5984
# obs = [true,false,true,true,true,true,true,false,false,true,false,false,true]
model = coins_learning(obs)
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:param)
show_var_dist_pct(chns,:ix)
show_var_dist_pct(chns,:ix1)
show_var_dist_pct(chns,:ix2)
show_var_dist_pct(chns,:ix3)
show_var_dist_pct(chns,:ix4)
show_var_dist_pct(chns,:ix5)
