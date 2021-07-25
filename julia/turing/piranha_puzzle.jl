#=
   The Piranha Puzzle in Turing.jl
   (From Tijms 2004)
   
   http://cs.ioc.ee/ewscs/2020/katoen/katoen-slides-lecture1.pdf
   """
   One fish is contained withing the confines of an opaque fishbowl.
   The fish is equally likely to be a piranha or a goldfish. A sushi
   lover throws a piranha into the fish bowl alongside the other fish.
   Then, immediately, before either fish can devour the other, one
   of the fish is blindly removed from the fishbowl. The fish that
   has been removed from the bowl turns out to be a piranha. What 
   is the probability that the fish that was originaly in the bowl
   by itself was a piranha?

   The Piranha Puzzle Program
   
   f1 := gf [0.5] f1 := pir;
   f2 := pir;
   s := f1 [0.5] s := f2;
   observe(s = pir)


   E(f1 = pir | P terminates) = 1/2 / 3/4 = 2/3
   """

   goldfish: 1 piranha: 2
   Distributions of variable f1 (num:0)
   2.00000 =>    6622  (0.662200)
   1.00000 =>    3378  (0.337800)

   The exact probability of first fish is a piranha is 0.6666

   Cf ~/webppl/piranha_puzzle.wppl

=#

using Turing
include("jl_utils.jl")


@model function piranha_puzzle()

    goldfish = 1
    piranha = 2

    f1 ~ Categorical([0.5,0.5])
    f2 = piranha
    fs = [f1,f2]
    s ~ Categorical([0.5,0.5])
    true ~ Dirac(fs[s] == piranha)

    f1_piranha ~ Dirac(f1 == piranha)
    
end

model = piranha_puzzle()

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(15), 10_000)
# chains = sample(model, SMC(), 10_000)
# chains = sample(model, IS(), 10_000)

display(chains)
println("goldfish: 1 piranha: 2")
show_var_dist_pct(chains,:f1) 



