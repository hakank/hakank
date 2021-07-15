#=
   Gender height.

   Identify a person's sex by height.

   Cf ~/cplint/gender_height.pl
      ~/blog/gender_height.blog
      ~/webppl/gender_height.wppl


   Note: The observations of height and gender seems to be much better
         using Turings standard "parameter observation" than using
         Dirac etc.

=#

# using Memoization
using Turing, StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)
include("jl_utils.jl")


@model function gender_height(gender=missing,height=missing)
# @model function gender_height()

    male = 1
    female = 2
    gender ~ Categorical([0.5,0.5])

    # From https://en.wikipedia.org/wiki/List_of_average_human_height_worldwide
    # Here are the values of Sweden. I'm not sure about the variance of these heights...
    height ~ gender == male ? Normal(181.5,sqrt(50)) : Normal(166.8,sqrt(50))

    # height ~ gender == male ? Normal(178,17.7) : Normal(163,17.3)

end

function run_model(gender=missing,height=missing)

    model = gender_height(gender,height)

    num_chains = 4

    # chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

    chains = sample(model, MH(), 10_000)
    # chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

    # chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, PG(20), 1_000)

    # chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, IS(), 10_000)

    # chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, SMC(1000), 10_000)


    # chains = sample(model, NUTS(1000,0.65), 1_000)
    # chains = sample(model, Gibbs(MH(:gender),NUTS(1000,0.65,:height)), 1_000)
    # chains = sample(model, Gibbs(MH(:gender),NUTS(10,0.65,:height)), 1_000)
    # chains = sample(model, Gibbs(MH(:gender),HMC(0.1,5,:height)), 1_000)
    # chains = sample(model, Gibbs(PG(10,:gender),HMC(0.1,5,:height)), 1_000)
    # chains = sample(model, Gibbs(MH(:gender),NUTS(1_000,0.65,:height)), 1_000)

    if ismissing(height)
        display(chains)
        # display(plot(chains))
    end
    show_var_dist_pct(chains, :gender)
    show_var_dist_pct(chains, :height, 20)
end

male = 1
female = 2
println("Male is coded as 1, female as 2.")
for height in [150.0,160.0,170.0,175.0,180.0, 185.0,190.0,200.0]
    println("\ngender:missing height:$height")
    run_model(missing,height)
    println()
end

println("\nmale, missing")
run_model(male,missing)

println("\nfemale, missing")
run_model(female,missing)
