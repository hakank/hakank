#=
   Tourist with a short memory

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   page 2, Problem 1.2

   A tourist wants to visits all the four cities, A, B, C, and D. 
   When he is in A he then visit the other cities (B, C, or D) 
   with equal probability, and the same for the other cities.
   Unfortunately, he don't remember which cities he has already visited
   so he always select the other 3 cities woth equal probability.

   How many cities will he visit until he had visited all four cities?

   The text state the "theoretical" expectation (geometry distribution) as:
   """
      E(N) = 1 + 1 + 3/2 + 3 = 13/2 [= 6.5]
   """

   Theoretical (numCities: 4): 6.5


   Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           len    6.4313    2.5985     0.0260    0.0293   6805.0938    1.0000     2741.7783

   Distributions of variable len (num:0)
   4.00000 =>    2315  (0.231500)
   5.00000 =>    2258  (0.225800)
   6.00000 =>    1749  (0.174900)
   7.00000 =>    1197  (0.119700)
   8.00000 =>     842  (0.084200)
   9.00000 =>     570  (0.057000)
   10.00000 =>     324  (0.032400)
   11.00000 =>     241  (0.024100)
   12.00000 =>     167  (0.016700)
   13.00000 =>     107  (0.010700)
   14.00000 =>      73  (0.007300)
   15.00000 =>      47  (0.004700)
   16.00000 =>      28  (0.002800)
   17.00000 =>      25  (0.002500)
   18.00000 =>      20  (0.002000)
   20.00000 =>       9  (0.000900)
   21.00000 =>       9  (0.000900)
   19.00000 =>       8  (0.000800)
   22.00000 =>       7  (0.000700)
   28.00000 =>       2  (0.000200)
   24.00000 =>       1  (0.000100)
   23.00000 =>       1  (0.000100)


  Cf ~/webppl/tourist_with_a_short_memory.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function tourist_with_a_short_memory(numCities=4)
    cities = [i for i in 1:numCities]

    function visitCities(a)
        len = length(a)
        if length(unique(a)) == numCities
            return a
        elseif len == 0
            c = rand(cities)
            return visitCities(push!(a,c))
        else 
            lastCity = a[end]
            nextCity = rand(setdiff(cities,[lastCity]))
            return visitCities(push!(a,nextCity))
        end
    end
    
    a = visitCities([])
    len ~ Dirac(length(a))
    
end

numCities = 4
theoretical_tourist = numCities -> 1 + (numCities-1)*sum([1/i for i in 1:numCities-1])
println("Theoretical (numCities: $numCities): ", theoretical_tourist(numCities))
model = tourist_with_a_short_memory(numCities)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:len)
