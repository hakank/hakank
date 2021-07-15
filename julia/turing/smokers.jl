#=

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/05_smokers.html
  """
  Social networks (Friends & Smokers)

  The ProbLog program below encodes a variant of the famous Friends & Smokers problem. The
  first two rules state that there are two possible causes for a person X to smoke,
  namely X having stress, and X having a friend Y who smokes himself and influences X.
  Note that, according to our this program, the presence of stress and of (possibly multiple)
  smoking friends all contribute to the probability of the person X smoking, namely in a
  noisy-or way (recall the noisy-or in the coin tossing example). Furthermore, the program
  encodes that if X smokes, (s)he has asthma with probability 0.4.

  The program below considers a scenario with 4 people, having a total of 5 friendship relations.
  """

  Cf ~/blog/smokers.blog
     ~/webppl/smokers.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function smokers(mode,pp1=1,pp2=1)
    people = [1,2,3,4]
    n = length(people)

    stress = tzeros(n)
    for p in 1:n
        stress[p] ~ flip(0.3)
    end

    influences = tzeros(n,n)
    for p1 in 1:n, p2 in 1:n
        if p1 != p2
            influences[p1,p2] ~ flip(0.2)
        else
            influences[p1,p2] ~ flip(0.0)
        end
    end

    friend_table = Dict(
        [1,2]=>1,
        [2,1]=>1,
        [2,4]=>1,
        [3,2]=>1,
        [4,2]=>1
    )
    friend = tzeros(n,n)
    for p1 in 1:n, p2 in 1:n
        friend[p1,p2] ~ get(friend_table,[[p1,p2]],0) == 1 ? flip(1.0) : flip(0.0)
    end

    smokes = tzeros(n)
    for p1 in 1:n
        smokes[p1] ~ flip(0.5)
    end

    for p1 in 1:n
        if stress[p1] == true
            smokes[p1] ~ flip(1.0)
        else
            if any(p2 -> p2 != p1 && friend[p1,p2]==1 && influences[p2,p1]==1 && smokes[p2]==1, people)
                smokes[p1] ~ flip(1.0)
            else
                smokes[p1] ~ flip(0.0)
            end
        end
    end

    asthma = tzeros(n)
    for p1 in 1:n
        if smokes[p1]==1
            asthma[p1] ~ flip(0.4)
        else
            asthma[p1] ~ flip(0.0)
        end
    end

    # Observations
    true ~ Dirac(smokes[2]==true)
    true ~ Dirac(influences[4,2]==true)

    if mode == "smokes"
        prob ~ Dirac(smokes[pp1])
        return prob 
    elseif mode == "asthma"
        prob ~ Dirac(asthma[pp1])
        return prob 
    elseif mode == "stress"
        prob ~ Dirac(stress[pp1])
        return prob 
    elseif mode == "influences"
        prob ~ Dirac(influences[pp1,pp2])
        return prob 
    else
        true
    end
    
end

function run_smokers(mode,pp1,pp2)
    model = smokers(mode,pp1,pp2)
    num_chains = 4

    # chains = sample(model, Prior(), 10_000)

    # chains = sample(model, MH(), 10_000)
    # chains = sample(model, PG(15), 1_000)
    # chains = sample(model, SMC(1000), 10_000)
    chains = sample(model, SMC(1000), 1_000)

    # chains = sample(model, IS(), 10_000)

    #
    # display(chains)
    # show_var_dist_pct(chains,:len,1000)

    # println("prob return value t[$a,$b]:")
    # genq = generated_quantities(model, chains)
    # show_var_dist_pct(genq,1000)
    chains_params = Turing.MCMCChains.get_sections(chains, :parameters)
    genq = generated_quantities(model, chains_params)
    
    show_var_dist_pct(chains,:prob)
    
    println("mean $(mean(genq))")
    return mean(genq)
end

println("SMOKES: ")
for p in 1:4
    println("\nsmokes($p)")
    run_smokers("smokes",p,p)
end

println("\nSTRESS: ")
for p in 1:4
    println("\nstress($p)")
    run_smokers("stress",p,p)
end


println("\nASTHMA: ")
for p in 1:4
    println("\nasthma($p)")
    run_smokers("asthma",p,p)
end

println("\nINFLUENCES: ")
influences = tzeros(4,4)
for p1 in 1:4, p2 in 1:4
    println("\ninfluences($p1,$p2)")
    influences[p1,p2] = run_smokers("influences",p1,p2)
end

display(influences)
