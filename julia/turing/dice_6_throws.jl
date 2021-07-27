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

# ix: Throw index
# val: The value to check
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

    a = throws(Int16[])
    len ~ Dirac(length(a))
    
    # Probability that the die lands on face val on throw ix
    test ~ Dirac(length(a) >= ix && a[ix] == val)
end




function run_model(ix, val)
    model = dice_6_throws(ix,val)

    num_chns = 4

    # chns = sample(model, Prior(), 10_000)

    # chns = sample(model, MH(), MCMCThreads(), 100_000, num_chns)
    # chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)

    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, MH(), 1_000)

    # chns = sample(model, PG(15), MCMCThreads(), 1_000, num_chns)

    # chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)
    # chns = sample(model, SMC(1000), 10_000)
    chns = sample(model, IS(), 10_000)

    display(chns)
    # display(plot(chns))

    show_var_dist_pct(chns,:test)
    show_var_dist_pct(chns,:len)    

end

for i in 1:10
    println("\nix:$i val:1")
    run_model(i,1)
end
