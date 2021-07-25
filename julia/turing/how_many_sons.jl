#=
   How many sons?

   From Ruma Falk:
   "Understanding Probability and Statistics - A Book of Problems"
   page 56
   """
   How many sons?

   Mrs. F is known to be the mother of two. You meet her in town
   with a boy whom she introduces as her son. What is the probability
   that Mrs. F has two sons? One possible is that the probability 
   is 1/2: you now have seen one boy, and there the question is 
   whether the other child is a male. Another possibility is that
   the probability is 1/3: you learned that Mrs. F has 'at least
   one boy', and hence three equiprobable family structures are
   possible (BB, BG, GB), of which the target event (BB) is but one.

   Which is the correct answer? Explain.

   (Hint: What are your assuptions about the chance mechanism that
   yielded your observation of Mrs. F hand her son?)
   """

   This problem (with a slighly different wording) was also in
   Maya Bar-Hillel & Ruma Falk: "Some teasers concerning conditional
   probabilities" (Cognition 11, 1982, page 109-122).


   Here we show these two different approaches:
   - model1: p(two_sons) = 1/2
   - model2: p(two_sons) = 1/3

   As commented in the hint (and the comments at page 177), the 
   assumptions regarding the "chance mechanism" - i.e. how we 
   know what - matters.

   Cf ~/webppl/how_many_sons.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


#=
In this model we know that the "first child" is a son.

Distributions of variable child2 (num:0)
1.00000 =>    5027  (0.502700)
2.00000 =>    4973  (0.497300)

=#
@model function how_many_sons1()
    son = 1
    daughter = 2
    
    child1 ~ Categorical([0.5,0.5]) # [son,daughter]
    child2 ~ Categorical([0.5,0.5])

    true ~ Dirac(child1 == son)

end

println("Model 1")
model = how_many_sons1()

num_chains = 4
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:child2)

#=
In this model we only know that one of the children
- but not which of them - is a son.

Distributions of variable child[2] (num:0)
1.00000 =>    6751  (0.675100)
2.00000 =>    3249  (0.324900)
=#

@model function how_many_sons2()
    son = 1
    daughter = 2

    child ~ filldist(Categorical([0.5,0.5]),2) # [son,daughter]
    
    # How many of the childrens are sons?
    numSons ~ Dirac(sum([child[i] == true ? 1 : 0 for i in 1:2]))

    # We know that at least one of the children is a son
    true ~ Dirac(numSons >= 1)

    # This don't work:
    # child1 ~ Categorical([0.5,0.5]) # [son,daughter]
    # child2 ~ Categorical([0.5,0.5])
    # true ~ Dirac((child1 == son ? 1 : 0) + (child2 == son ? 1 : 0) >= 1)

end

println("\nModel 2")
model = how_many_sons2()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))

show_var_dist_pct(chns,Symbol("child[2]"))
