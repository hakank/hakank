#=

  Assignment 1:
  https://edu.swi-prolog.org/mod/assign/view.php?id=242
  """
  http://cplint.eu/p/urns_and_balls.swinb

  Urns and balls

  Suppose you have two urns: urn1 contains 40 blue balls and 20 red balls and urn2 contains 25
  blue balls and 30 red balls.

  You throw an unbiased coin and, if it turns out head, you draw a ball from the first urn,
  it it turns out tails you draw a ball from the second urn.

  Write a program modeling this process and a query for answering the question
  "What is the probability of drawing a blue ball?"

  Write the program and the query in the cells below. Then save the notebook and submit the url.

  """
  The exact answer of drawing a blue call is
     0.5*40/60 + 0.5*25/55 = 0.56060606060606060606


  Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

        coin    1.5019    0.5000     0.0025    0.0025   40549.0446    1.0000    28840.0032
        draw    1.4389    0.4963     0.0025    0.0023   40570.3731    1.0000    28855.1729

  Distributions of variable draw
  blue       =>   22442  (0.561050)
  red        =>   17558  (0.438950)

  Distributions of variable coin
  head       =>   20078  (0.501950)
  tail       =>   19922  (0.498050)


  Cf ~/cplint/course_urns_and_balls.pl
     ~/blog/urns_and_balls.blog
     ~/psi/urns_and_balls.blog
     ~/webppl/urns_and_balls.wppl

=#

using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)
include("jl_utils.jl")

@model function urns_and_balls()
    tail = 1
    head = 2
    coin ~ Categorical(simplex([0.5,0.5]))

    blue = 1
    red = 2
    draw ~ coin == head ? Categorical(simplex([40,20])) : Categorical(simplex([25,30]))
end

model = urns_and_balls()

num_chains = 4
# chains = sample(model, Prior(), MCMCThreads(), 1000, num_chains)

# chains = sample(model, MH(), 40_000)
# chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 40_000)
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
chains = sample(model, IS(), 40_000)

display(chains)

show_var_dist_pct(chains,:draw,["blue","red"])
show_var_dist_pct(chains,:coin,["tail","head"])
