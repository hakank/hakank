#=
   Classical Random Walk 1

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 6f, Problem 1.5 Classical Random Walk 1, 
   cases a) Probability of absorption
         b) Expected number of steps until absorption
         c) Ruin problem

   The probability of moving to the left or right is 0.5 (symmetric
   random walk). The walk stops when reaching either -a or +b (
   a and are both positive integers).

   * a) and b)  a:10 and b:10
     a:10 b:10
     Theoretical prob of [-a,b]:[0.5, 0.5]
     Theoretical length of absorption:100

     Summary Statistics
      parameters       mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
           Symbol    Float64   Float64    Float64   Float64      Float64   Float64       Float64 

             len   100.6312   82.2539     0.8225    0.7312   10156.6554    0.9999     8164.5140
         lastPos     0.0460   10.0004     0.1000    0.0915    9771.6168    1.0000     7854.9975


     Distributions of variable lastPos (num:0)
     10.00000 =>    5023  (0.502300)
     -10.00000 =>    4977  (0.497700)

   * c) The ruin problem, a=1 and b=100:

     a:1 b:100
     Theoretical prob of [-a,b]:[0.9900990099009901, 0.009900990099009901]
     Theoretical length of absorption:100

     Summary Statistics
     parameters       mean        std   naive_se      mcse          ess      rhat   ess_per_sec 
         Symbol    Float64    Float64    Float64   Float64      Float64   Float64       Float64 

            len   110.7316   612.2311     6.1223    6.2518    9854.2103    0.9999    62765.6706
        lastPos     0.0504    10.2468     0.1025    0.1034   10218.6345    0.9999    65086.8437


    Distributions of variable lastPos (num:0)
    -1.00000 =>    9896  (0.989600)
    100.00000 =>     104  (0.010400)

  Cf ~/webppl/random_walk_1.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

random_walk_1_theoretical_prob = (a,b) -> [b/(a+b),a/(a+b)]
random_walk_1_theoretical_length = (a,b) -> a*b

@model function random_walk_1(a=10,b=10)
    t = [-1,1]
    function walk(arr) 
        len = length(arr)
        last = len == 0 ? 0 : arr[end]
        if last == -a || last == b
            return arr
        else
            # d ~ UniformDraw([-1,1])
            d = rand(t)
            return walk(push!(arr,last + d) )
        end
    end
    
    arr = walk([])
    len ~ Dirac(length(arr))
    lastPos ~ Dirac(arr[end])
        
    return arr
end

function run_random_walk(a,b)
    println("a:$a b:$b")
    println("Theoretical prob of [-a,b]:", random_walk_1_theoretical_prob(a,b))
    println("Theoretical length of absorption:", random_walk_1_theoretical_length(a,b))
    model = random_walk_1(a,b)

    chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    # chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 10_000)

    display(chns)
    # display(plot(chns))
    show_var_dist_pct(chns, :lastPos)
    
    # chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
    # genq = generated_quantities(model, chains_params)
    # show_var_dist_pct(genq)
    println()
end

run_random_walk(10,10)
run_random_walk(1,100)
