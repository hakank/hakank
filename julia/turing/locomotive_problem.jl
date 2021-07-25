#=
   Locomotive problem.

   From Think Bayes, page 22ff
   """
   I found the locomotive problem in Frederick Mosteller's "Fifty Challenging
   Problems in Probability with Solutions" (Dover, 1987):
      'A railroad numbers its locomotives in order 1..N. One day you
       see a locomotive with the number 60. Estimate how many loco-
       motives the railroad has.'

   ...

   The mean of the posterior is 333, so that might be a good guess if you wanted to
   minimize error.
   """

   As the book (Think Bayes) mentions, the model is very sensitive to 
   the value of maxInt. Here are some results for different maxInt:
   

   Here are the values from this model for different values of maxInt 
   (using MH())
     maxInt:100  mean(n): 78.23232
     maxInt:200  mean(n): 115.87732
     maxInt:500  mean(n): 206.42599
     maxInt:1000  mean(n): 335.66271
     maxInt:2000  mean(n): 552.68673

   These are the exact values from WebPPL model using the enumerate sampler.
   maxInt  n
   -------------------------
    100    77.8166306057923
    200   115.84577808279282
    500   207.2393675826458
   1000   334.04414386751915 
   2000   553.5237331955558

   Mosteller's solution: 2*(60-1)+1: 119.

  cf german_tank.jl
     german_tank_int.jl
     ~/blog/locomotive_problem.blog
     ~/webppl/locomotive_problem.wppl
=#

using Turing
include("jl_utils.jl")

@model function locomotive_problem(ys,maxInt=1000)
    len = length(ys)
    n ~ DiscreteUniform(1,maxInt)
    ys ~ filldist(DiscreteUniform(1,n),len)
end

# Run the model with different values of maxInt
function run_locomotive_problem(ys,maxInt)
   model = locomotive_problem(ys,maxInt)

    # chns = sample(model, Prior(), 100_000)
    chns = sample(model, MH(), 100_000)
    # chns = sample(model, PG(15), 10_000)
    # chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 100_000)
    # display(chns)

    println("maxInt:$maxInt  mean(n): ", mean_val(chns,:n))
end

ys = [60]
for maxInt in [100,200,500,1000,2000]
    run_locomotive_problem(ys,maxInt)
end
