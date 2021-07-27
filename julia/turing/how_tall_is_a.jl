#=


  https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
  """
  Here are a series of problems I posed in my Bayesian statistics class:

  1) Suppose you meet an adult resident of the U.S. who is 170 cm tall. 
     What is the probability that they are male?

  2) Suppose I choose two U.S. residents at random and A is taller than B.  
     How tall is A?

  3) In a room of 10 randomly chosen U.S. residents, A is the second tallest.  
     How tall is A?  
     And what is the probability that A is male?

  As background: For adult male residents of the US, the mean and standard deviation of 
  height are 178 cm and 7.7 cm. For adult female residents the corresponding stats 
  are 163 cm and 7.3 cm.  And 51% of the adult population is female.

  """

  Cf ~/blog/how_tall_is_a.blog
     ~/webppl/how_tall_is_a.wppl

=#


using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
Turing.setadbackend(:forwarddiff)
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:tracker)
# Turing.setadbackend(:zygote)

include("jl_utils.jl")

#=
  Model 1:

  1) Suppose you meet an adult resident of the U.S. who is 170 cm tall. 
  What is the probability that they are male?
  
  According to https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
    female 0.5432225483131837
    male 0.45677745168681627

  This model:
  Distributions of variable gender (num:0)
    2.00000 =>    5406  (0.540600)
    1.00000 =>    4594  (0.459400)
=#
@model function how_tall_is_1(height=170)
    male = 1
    female = 2
    gender ~ Categorical(simplex([0.49,0.51]))
    height ~ gender == male ? Normal(178,7.7) : Normal(163,7.3)

    # true ~ Dirac(height == 170.0)
    
end

#=
println("Model 1")
model = how_tall_is_1(170)
# chns = sample(model, Prior(), 10_000)
chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 1_000)
# chns = sample(model, SMC(1000), 10_000)
# chns = sample(model, IS(), 10_000)

#chns = sample(model, NUTS(), 1000)
# chns = sample(model, HMC(0.1, 5), 1000)
# chns = sample(model, HMCDA(0.15, 0.65), 1000) 
# chns = sample(model, Gibbs(MH(:gender),NUTS(1000,0.65,:height)), 1_000)

display(chns)
println("male: 1 female: 2")
show_var_dist_pct(chns,:gender)
# show_var_dist_pct(chns,:height,20)
=#


#=

  2) Suppose I choose two U.S. residents at random and A is taller than B.  
  How tall is A?

  Solution from https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
  A: 176.67506663725212
  B: 164.05298129722925

  This model:
    ...
    height[1]   176.4384    9.0135     0.2850    0.1777   824.6929    1.0007      179.5152
    height[2]   164.5469    8.4415     0.2669    0.2017   828.7247    0.9998      180.3928

=#


@model function how_tall_is_2()
    male = 1
    female = 2
    gender = tzeros(2)
    height = tzeros(2)
    for p in 1:2
        gender[p] ~ Categorical(simplex([0.49,0.51]))
        height[p] ~ gender[p] == male ? Normal(178,7.7) : Normal(163,7.3)
    end
    true ~ Dirac(height[1] > height[2])
    
end

#=
println("\n\nModel 2")
model = how_tall_is_2()
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(15), 1_000)
# chns = sample(model, SMC(1000), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
println("male: 1 female: 2")
show_var_dist_pct(chns,Symbol("gender[1]")) 
show_var_dist_pct(chns,Symbol("gender[2]"))                
=#

#= 

  3) In a room of 10 randomly chosen U.S. residents, A is the second tallest.  
  How tall is A?  
  And what is the probability that A is male?
  
    Solution from https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
    A: 181.60660153115973
  

=#

# Note:
# This model does NOT work as expected. It's fine for n=2 (see how_tall_is_2), but not the
# generalized version of n=10. See how_tall_is_3_2() for a working model (thanks to Seth Axen).
# See the discussion here: 
#  https://discourse.julialang.org/t/help-with-turing-jl-model-using-categorical-normal-and-ordered-constraint/64765
#
@model function how_tall_is_3()
    male = 1
    female = 2
    n = 10
    gender = tzeros(n)
    height = tzeros(n)
    for p in 1:n
        gender[p] ~ Categorical(simplex([0.49,0.51]))
        height[p] ~ gender[p] == male ? Normal(178,7.7) : Normal(163,7.3)
    end

    for p in 1:n-1
        # true ~ Dirac(height[p] > height[p+1])
        height[p] > height[p+1] || begin Turing.@addlogprob! -Inf; end
        # height[p] > height[p+1] || begin Turing.@addlogprob! -2; end
    end
    
    a_is_male ~ Dirac(gender[2] == male)
end

#=
println("\n\nModel 3")
model = how_tall_is_3()
# chns = sample(model, Prior(), 10_000)
chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)
display(chns)
println("male: 1 female: 2")
show_var_dist_pct(chns,:a_is_male) 
=#

#
# Thanks to Seth Axen for this approach.
# From https://discourse.julialang.org/t/help-with-turing-jl-model-using-categorical-normal-and-ordered-constraint/64765/2
# Note that the heights[i] are all about 170cm; it's the height_ordered that we are
# interested in:
#=
   height_ordered[1]   186.706
   height_ordered[2]   181.5520
   height_ordered[3]   177.8786
   height_ordered[4]   174.6669
   height_ordered[5]   171.6923
   height_ordered[6]   168.7933
   height_ordered[7]   165.8899
   height_ordered[8]   162.8638
   height_ordered[9]   159.4338
  height_ordered[10]   154.5771
            height_A   181.5520
          isfemale_A     0.0893
=#
@model function how_tall_is_3_2()
    male = 1
    female = 2
    n = 10
    # This is from Seth Axen:
    isfemale ~ filldist(Bernoulli(0.51), n)
    height = tzeros(n) # Vector{T}(undef, n)
    for i in 1:n
        if isfemale[i]
            height[i] ~ Normal(163, 7.3)
        else
            height[i] ~ Normal(178, 7.7)
        end
    end
    # hakank: I added this since I actually wanted the
    #         weights of all people.
    height_ordered = tzeros(n) # Vector{T}(undef, n)
    perm = sortperm(height)
    for i in 1:n
        height_ordered[i] ~ Dirac(height[perm[n-i+1]])
    end

    # From Seth:
    id_A = perm[n-1]
    height_A ~ Dirac(height[id_A])
    isfemale_A ~ Dirac(isfemale[id_A])
    
end

model = how_tall_is_3_2()
# Comment from Seth:
# "no inference necessary! We just sample directly from the prior-predictive distribution"
chns = sample(model, Prior(), 10_000)
display(chns)
show_var_dist_pct(chns,:isfemale_A) 
# Testing with IS(), same result
# chns = sample(model, IS(), 10_000)
# display(chns)
