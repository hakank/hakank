#=
   1 RM Calculations (strength training)

   See https://en.wikipedia.org/wiki/One-repetition_maximum

   Here we calculate two versions of 1 RM (1 repetition), 
   adjusted for reps and also adjusted for age (which in my  
   case is necessary :-))

   For age:64 weight:102 reps:5 we get

         parameters       mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
             Symbol    Float64   Float64    Float64   Float64      Float64   Float64       Float64 

        onerm_epley   119.0048    0.5035     0.0050    0.0049   10111.3967    0.9999     5230.9347
    onerm_age_epley   193.8589    0.8203     0.0082    0.0080   10111.3967    0.9999     5230.9347
      onerm_brzycki   114.7478    0.5052     0.0051    0.0057    9583.0426    1.0007     4957.6009
  onerm_age_brzycki   186.9242    0.8230     0.0082    0.0092    9583.0426    1.0007     4957.6009

  The age adjustments should be interpreted as the weight a person at age 30 would have to lift.

  And for my deadlift PR at 1x117kg: age:64 weight:117 reps:1

         parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
             Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        onerm_epley   120.9058    0.5066     0.0051    0.0050   9629.8855    1.0000    10031.1308
    onerm_age_epley   196.9556    0.8252     0.0083    0.0082   9629.8855    1.0000    10031.1308
      onerm_brzycki   116.9990    0.4978     0.0050    0.0061   9253.4812    1.0002     9639.0429
  onerm_age_brzycki   190.5914    0.8110     0.0081    0.0099   9253.4812    1.0002     9639.0429

  So, for my 1 rep 117kg lift, a person at age would have to lift about 190kg to be as strong as I am
  now (at age 64). 


   Cf ~/blog/OneRM.blog
     ~/webppl/OneRM.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")


@model function one_rm(age=63,weight=102,reps=5)
    age_coeff = Dict(
         30=> 1.000, 31=> 1.016, 32=> 1.031, 33=> 1.046, 34=> 1.059,
         35=> 1.072, 36=> 1.083, 37=> 1.096, 38=> 1.109, 39=> 1.122,
         40=> 1.135, 41=> 1.149, 42=> 1.162, 43=> 1.176, 44=> 1.189,
         45=> 1.203, 46=> 1.218, 47=> 1.233, 48=> 1.248, 49=> 1.263,
         50=> 1.279, 51=> 1.297, 52=> 1.316, 53=> 1.338, 54=> 1.361,
         55=> 1.385, 56=> 1.411, 57=> 1.437, 58=> 1.462, 59=> 1.488,
         60=> 1.514, 61=> 1.541, 62=> 1.568, 63=> 1.598, 64=> 1.629,
         65=> 1.663, 66=> 1.699, 67=> 1.738, 68=> 1.779, 69=> 1.823,
         70=> 1.867, 71=> 1.910, 72=> 1.953, 73=> 2.004, 74=> 2.060,
         75=> 2.117, 76=> 2.181, 77=> 2.255, 78=> 2.336, 79=> 2.419,
         80=> 2.540, 81=> 2.597, 82=> 2.702, 83=> 2.831, 84=> 2.981,
         85=> 3.153, 86=> 3.352, 87=> 3.580, 88=> 3.843, 89=> 4.145,
         90=> 4.493
        )
    
    weight ~ Uniform(1,200)
    reps   ~ DiscreteUniform(1,30)
    age    ~ Uniform(30,90)

    # OneRM adjusted for reps
    onerm_epley ~ Normal(weight*(1.0 + reps/30.0),0.5)
    
    # OneRM adjusted for age
    onerm_age_epley ~ Dirac(onerm_epley * age_coeff[round(Int,age)])

    # OneRM adjusted for reps
    onerm_brzycki ~ Normal(weight * (36.0/(37.0 - reps)),0.5)

    # OneRM adjusted for age
    onerm_age_brzycki ~ Dirac(onerm_brzycki * age_coeff[round(Int,age)])

end



function run_one_rm(age,weight,reps)
    println("age:$age weight:$weight reps:$reps")
    model = one_rm(age,weight,reps)
    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 10_000)

    display(chns)
    # display(plot(chns))
    println()
end

age = 64
weight=102
reps=5
run_one_rm(age,weight,reps)

age = 64
weight=117
reps=1
run_one_rm(age,weight,reps)
