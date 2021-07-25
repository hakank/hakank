#=

   Number of walks until no shoes

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 8f, Problem 1.6 Number of walks until no shoes

   """
   A has a house with one front door and one back door. He places
   two pairs of walking shoes at each door. For each walk, he selects
   one door at random, puts on a pair of shoes, returns after a walk
   to a randomly chosen door, and takes off the shoes at the door.
   We want to determine the expected number of finished walks until
   A discovers that no shoes are available at the door he has selected
   for his next walk.
   """

  For two pair of shoes, the theoretical probability. of winning for each strategy is 12.

   Summary Statistics
   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

         len   12.1993   10.1602     0.1016    0.1316   6606.4119    0.9999     2942.7225


   Theoretical walks for n: 1:10:
   (1, 4)
   (2, 12)
   (3, 24)
   (4, 40)
   (5, 60)
   (6, 84)
   (7, 112)
   (8, 144)
   (9, 180)
   (10, 220)


  Cf  banachs_match_box_problem.jl
      ~/webppl/number_of_walks_until_no_shoes.wppl

=#

using Turing, StatsPlots
include("jl_utils.jl")

# The theoretical probability
function theoreticalProb2(n,i)
    2*n+4*n*i-2*i*i
end

@model function number_of_walks_until_no_shoes(n,i)

    # In both function we use rand(Bernoulli()), not
    #   flip()
    # since that don't give the proper solution.
    
    # Take shoes from a random door
    # Note: we only add to a when taking the shoes.
    function takeShoes(a,left,right) 
        # Pick a door and check if there are any shoes
        pick = rand(Bernoulli()) == true ? 'l' : 'r'
        if (pick == 'l' && left == 0)  || (pick == 'r' && right == 0)
            return a
        else 
            if pick == 'l'
                return leaveShoes(push!(a,pick),left-1,right)
            else
                return leaveShoes(push!(a,pick),left,right-1)
            end
        end
    end
    
    # Leave shoes at a random door
    function leaveShoes(a,left,right)
        if rand(Bernoulli()) == true 
            return takeShoes(a,left+1,right)
        else 
            return takeShoes(a,left,right+1)
        end
    end
    
    a = takeShoes([],n,i)
    
    len ~ Dirac(length(a))
                
    return a
                
end

n = 2
i = 2
# n = 10
# i = 10
println("Theoretical prob:", theoreticalProb2(n,i))
model = number_of_walks_until_no_shoes(n,i)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

# show_var_dist_pct(chns, :len)

# chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
# genq = generated_quantities(model, chains_params)
# show_var_dist_pct(genq)
# println()
    
println("\nTheoretical walks for n: 1:10:")
for i in 1:10
    println((i,theoreticalProb2(i,i)))
end
