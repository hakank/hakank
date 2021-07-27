#=
  https://www.youtube.com/watch?v=e1Ykk_CqKTY&t=458s

  Probabilistic Programming: What It Is and How It Works - Noel Welsh

  We can see either 1, 2, or 3 cats.
  There are 3 different enticements:

  - Milkshake
  - Fish
  - Nothing

  And there are different probabilities how many cats there are given
  an enticement, see below.

  Now: We see 3 cats, what is the probability that it's a milkshake?

  The video got the following (for 3 cats):
   - milkshake: 0.42
   - fish: 0.04
   - nothing: 0.03

  Normalized to percentage (from the video):

  0.42/(0.42 + 0.04 + 0.03) milkshake
      0.85714285714285714286
  0.04/(0.42 + 0.04 + 0.03) fish
      0.081632653061224489796
  0.03/(0.42 + 0.04 + 0.03)  nothing
      0.061224489795918367347

  Here are two models.
  Model2 seems to be the correct one (i.e. corresponds to the video),
  i.e. where the enticements are mutual exclusive.
  In model1 we accept that fish and milkshake can coexist.

  See ~/cplint/cat.pl
      ~/blog/cat.blog
      ~/psi/cat.psi
=#

using Turing # , StatsPlots, DataFrames
include("jl_utils.jl")
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)


# This is not correct
@model function cat1()
    NoCat = 0
    OneCat = 1
    TwoCats = 2
    ThreeCats = 3

    # probability of an enticement in the garden
    milkshake      ~ Bernoulli(0.6)
    fish           ~ Bernoulli(0.1)
    no_enticement  ~ Bernoulli(0.3)

    # Number of cats per enticement
    vs = [NoCat,OneCat,TwoCats,ThreeCats]
    vs_cat ~ DiscreteUniform(NoCat,ThreeCats)
    if !no_enticement && milkshake
        cat ~ Categorical(simplex([0.0,0.1,0.2,0.7]))
    elseif !no_enticement && fish
        cat ~ Categorical(simplex([0.0,0.2,0.3,0.4]))
    elseif no_enticement && !fish && !milkshake
        cat ~ Categorical(simplex([0.0,0.6,0.3,0.1]))
    else
        cat ~ Categorical(simplex([1.0,0.0,0.0,0.0]))
    end
    true ~ Dirac(vs_cat == vs[cat]) 
    true ~ Dirac(vs_cat == ThreeCats)

end

#=
println("Model 1:")
model = cat1()
num_chns = 4
# chns = sample(model, Prior(), MCMCThreads(), 1000, num_chns)
chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, MH(), 1000)
display(chns)
=#



# This is correct (according to the video)
#=
   Summary Statistics
            parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

            enticement    1.2108    0.5429     0.0014    0.0093   2253.8077    1.0013       70.4381
  enticement_milkshake    0.8532    0.3539     0.0009    0.0061   2150.1791    1.0013       67.1994
       enticement_fish    0.0824    0.2750     0.0007    0.0046   2344.6207    1.0014       73.2763
    enticement_nothing    0.0642    0.2452     0.0006    0.0041   2630.1451    1.0013       82.1997
                  cats    2.9992    0.0403     0.0001    0.0008   2814.4694    1.0013       87.9604


   Distributions of variable enticement
   milkshake  =>  136546  (0.853413)
   fish       =>   13181  (0.082381)
   no_enticement =>   10273  (0.064206)

=#
enticements = []
enticements_dict = Dict(1=>"milkshake",2=>"fish",3=>"no_enticement")
@model cat2() = begin

    milkshake = 1
    fish = 2
    no_enticement = 3

    # Priors
    # probability of an enticement in the garden
    enticement ~ Categorical(simplex([0.6,0.1,0.3]))
    # Note: This seems a little too complex...
    enticement_milkshake ~ Bernoulli(0.5)
    enticement_fish      ~ Bernoulli(0.5)
    enticement_nothing   ~ Bernoulli(0.5)
    cats ~ DiscreteUniform(1,3)
    if enticement == milkshake
        cats ~ Categorical(simplex([0.1,0.2,0.7]))
        enticement_milkshake ~ Bernoulli(1.0)
        enticement_fish      ~ Bernoulli(0.0)
        enticement_nothing   ~ Bernoulli(0.0)
    elseif enticement == fish
        cats ~ Categorical(simplex([0.2,0.4,0.4]))
        enticement_milkshake ~ Bernoulli(0.0)
        enticement_fish      ~ Bernoulli(1.0)
        enticement_nothing   ~ Bernoulli(0.0)
    else
        cats ~ Categorical(simplex([0.6,0.3,0.1]))
        enticement_milkshake ~ Bernoulli(0.0)
        enticement_fish      ~ Bernoulli(0.0)
        enticement_nothing   ~ Bernoulli(1.0)
    end

    # We observe 3 cats
    true ~ Dirac(cats == 3)

    return enticement
end

println("\n\nModel 2:")
model = cat2()
num_chns = 4
# chns = sample(model, Prior(), MCMCThreads(), 1000, num_chns)
chns = sample(model, MH(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, MH(), 40_000)
# chns = sample(model, PG(20), MCMCThreads(), 1000, num_chns)
# chns = sample(model, SMC(), MCMCThreads(), 1000, num_chns)
# chns = sample(model, IS(), MCMCThreads(), 1000, num_chns)

display(chns)

show_var_dist_pct(chns, :enticement,["milkshake","fish","no_enticement"])

