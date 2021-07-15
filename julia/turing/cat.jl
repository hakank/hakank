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
num_chains = 4
# chains = sample(model, Prior(), MCMCThreads(), 1000, num_chains)
chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(), 1000)
display(chains)
=#



# This is correct (according to the video)
#=
Summary Statistics
            parameters      mean       std   naive_se      mcse         ess      rhat
                Symbol   Float64   Float64    Float64   Float64     Float64   Float64

                  cats    3.0000    0.0000     0.0000    0.0000         NaN       NaN
            enticement    1.2009    0.5322     0.0013    0.0090   2218.9994    1.0026
       enticement_fish    0.0783    0.2686     0.0007    0.0043   2703.4937    1.0026
  enticement_milkshake    0.8600    0.3470     0.0009    0.0058   2345.7527    1.0017
    enticement_nothing    0.0617    0.2407     0.0006    0.0041   2176.3555    1.0048
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

    # println("enticement:$(enticements_dict[$enticement]) cats:$cats")

    # We observe 3 cats
    true ~ Dirac(cats == 3) # || begin Turing.@addlogprob! -Inf; return; end
    # push!(enticements,[enticements_dict[enticement],cats])

    return enticement
end

println("\n\nModel 2:")
model = cat2()
num_chains = 4
# chains = sample(model, Prior(), MCMCThreads(), 1000, num_chains)
chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, MH(), 40_000)

# chains = sample(model, PG(20), MCMCThreads(), 1000, num_chains)

display(chains)

println("\nEnticement: milkshake:1 fish:2 no_enticement:3")
show_var_dist_pct(chains, :enticement)
