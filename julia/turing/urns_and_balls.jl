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

#=
Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64

        coin    1.5008    0.5000     0.0025    0.0025   39377.7877    1.0000
        draw    1.4408    0.4965     0.0025    0.0025   39019.4117    1.0000

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

        coin    1.0000    1.0000    2.0000    2.0000    2.0000
        draw    1.0000    1.0000    1.0000    2.0000    2.0000

length(data): 40001
Dict{Any,Any} with 4 entries:
  ["tail", "blue"] => 8991
  ["head", "red"]  => 6654
  ["tail", "red"]  => 10979
  ["head", "blue"] => 13377
p(tail):0.49923751906202346
p(blue):0.5591860203494913
  2.282226 seconds (8.63 M allocations: 532.511 MiB, 7.70% gc time)

=#

data = []
@model function urns_and_balls()
    tail = 1
    head = 2
    coin ~ Categorical(simplex([0.5,0.5]))

    blue = 1
    red = 2
    draw ~ coin == head ? Categorical(simplex([40,20])) : Categorical(simplex([25,30]))
    push!(data,[
                coin == tail ? "tail" : "head"
                draw == blue ? "blue" : "red"
    ])
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

# chains = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chains)

display(chains)
println("length(data): ", length(data))

h = make_hash(data)
display(h)
# println("data:", make_hash(data))
# println("p(blue): ", (df[!:,2] .== 2)/length(data))

println("p(tail):", sum([d[1] == "tail" for d in data])/length(data))
println("p(blue):", sum([d[2] == "blue" for d in data])/length(data))

# df = DataFrame(chains)
