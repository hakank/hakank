#=
    http://cplint.eu/example/inference/dice.swinb

    """
    A six-sided die is repeatedly thrown until the outcome is six. on(T,F)
    means that on the Tth throw the face F came out.
    """

    What is the probability that the die lands on face 1 at time 0?

    Cf ~/blog/dice_6_throws.blog
     ~/psi/dice_6_throws.psi
     ~/webppl/dice_6_throws.wppl

     Also see the related problem rolling_dice4.jl where we investigate
     the probability of the length of this (stopping) sequence.

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function dice_6_throws(ix=1,val=1)

    max_len = 100
    function throws(a)
        # Note: If t is a random variable then we got StackOverflowError
        # t ~  DiscreteUniform(1,6) # Don't work!
        # Hence we draw a "plain" random value
        t = rand(DiscreteUniform(1,6))
        if t == 6
            return a
        else
            return throws(vcat(a,t))
        end
    end

    # len is a pure random variable, though
    len ~ DiscreteUniform(0,max_len)
    a = throws(Int16[])
    len = length(a)

    # true ~ Dirac(length(a) >= 1 && a[1] == 1)
    # true ~ Dirac(length(a) >= 2 && a[2] == 1)

    # return len
    # Probability of
    return length(a) >= ix && a[ix] == val
    # return length(a) >= 1 && a[1] == 1
end




function run_model(ix, val)
    model = dice_6_throws(ix,val)

    num_chains = 4

    # chains = sample(model, Prior(), 10_000)

    # chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
    # chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)

    chains = sample(model, MH(), 10_000)
    # chains = sample(model, MH(), 1_000)

    # chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)

    # chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, SMC(1000), 10_000)
    # chains = sample(model, IS(), 10_000)
    #
    #chains = sample(model, Gibbs(HMC(0.1,5,:a,:b),PG(15,:p)), 10_000)
    # chains = sample(model, Gibbs(NUTS(1000,0.65,:a,:b),PG(15,:p)), 10_000)
    # chains = sample(model, Gibbs(HMC(0.1,5,:a,:b),SMC(1000,:p)), 10_000) # Nope

    # display(chains)
    # display(plot(chains))
    # show_var_dist_pct(chains,:len,1000)

    println("\nprob return value:")
    genq = generated_quantities(model, chains)
    show_var_dist_pct(genq,1000)

end

for i in 1:10
    println("\nix:$i val:1")
    run_model(i,1)
end
