#=
  From https://dtai.cs.kuleuven.be/problog/tutorial/mpe/01_bn.html
  (from ~/problog/bayesian_networks.pl )

  See ~/blog/bayesian_network.blog
      ~/psi/bayesian_network.psi
      ~/webppl/bayesian_network.wppl
=#

using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")


@model function bayesian_network()

    burglary ~ flip(0.7)
    earthquake ~ flip(0.2)

    alarm ~ flip(0.5)

    if burglary && earthquake
        alarm ~ flip(0.9)
    elseif burglary && !earthquake
        alarm ~ flip(0.8)
    elseif !burglary && earthquake
        alarm ~ flip(0.1)
    else
        alarm ~ flip(0.0); # false
    end

    # x calls depending on the alarm
    # But using function don't seems to work!
    # function calls(x)
    #    return alarm ? flip(0.8) : flip(0.1)
    # end
    # calls(john) == false ||  begin Turing.@addlogprob! -Inf; return; end
    # calls(mary) == trye ||  begin Turing.@addlogprob! -Inf; return; end

    calls_john ~ alarm ? flip(0.8) : flip(0.1)
    calls_mary ~ alarm ? flip(0.8) : flip(0.1)

    true ~ Dirac(calls_john == false)
    true ~ Dirac(calls_mary == true)

    return
end

model = bayesian_network()
num_chns = 4
# chns = sample(model, Prior(), MCMCThreads(), 1000, num_chns)
chns = sample(model, MH(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, PG(20), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, IS(), MCMCThreads(), 10_000, num_chns)

# chns = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chns)

display(chns)
