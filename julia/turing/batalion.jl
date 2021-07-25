#=
   From
   "First-Order Probabilistic Inference"
   https://www.youtube.com/watch?v=leIqVD4-Fks
   Time 45:06

   (The shown model in the talks is a BLOG model).


   numSoldierMean: 1095.9922 

   Distributions of variable numBatalion (num:0)
   6.00000 =>    1058  (0.105800)
   2.00000 =>    1028  (0.102800)
   5.00000 =>    1021  (0.102100)
   1.00000 =>    1020  (0.102000)
   7.00000 =>    1019  (0.101900)
   10.00000 =>    1018  (0.101800)
   9.00000 =>     969  (0.096900)
   4.00000 =>     958  (0.095800)
   3.00000 =>     955  (0.095500)
   8.00000 =>     954  (0.095400)



   cf ~/blog/batallion.blog
      ~/webppl/batallion.wppl

=#

using Turing
include("jl_utils.jl")

@model function batalion()
    numBatalion ~ DiscreteUniform(1,10)
    large ~ filldist(flip(0.6),numBatalion)
    region ~ filldist(Poisson(numBatalion),numBatalion)
    numSoldier = tzeros(numBatalion)
    for bat in 1:numBatalion
        if large[bat]
            numSoldier[bat] ~ Poisson(1500)
        else
            if region[bat] == 2
                numSoldier[bat] ~ Uniform(300,301) # Dirac(300)
            else
                numSoldier[bat] ~ Poisson(500)
            end
        end
    end
    numSoldierMean ~ Dirac(sum(numSoldier) / numBatalion)
end

model = batalion()

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
chs = sample(model, IS(), 10_000)
#chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

show_var_dist_pct(chs,:numBatalion)

