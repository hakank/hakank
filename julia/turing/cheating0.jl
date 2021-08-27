#=
    Cheating model.

    The model is a more verbose implementation of the Cheating problem in
    https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter2_MorePyMC/Ch2_MorePyMC_PyMC3.ipynb
    """
    What we need is a better algorithm to ask students if they had cheated. Ideally the algorithm 
    should encourage individuals to be honest while preserving privacy. The following proposed algorithm 
    is a solution I greatly admire for its ingenuity and effectiveness:

        In the interview process for each student, the student flips a coin, hidden from the interviewer. 
        The student agrees to answer honestly if the coin comes up heads. Otherwise, if the coin comes 
        up tails, the student (secretly) flips the coin again, and answers "Yes, I did cheat" if the 
        coin flip lands heads, and "No, I did not cheat", if the coin flip lands tails. This way, the 
        interviewer does not know if a "Yes" was the result of a guilty plea, or a Heads on a second 
        coin toss. Thus privacy is preserved and the researchers receive honest answers.
    """

    Compared to cheating.jl we add an array (cheated) which collected the students 
    that we think cheated. Of course it's just a simulation since we cannot pinpoint 
    the specific students that actually cheated. 
    
    But the model works and give the same results as cheating.jl (and cheating2.jl), i.e. that 
    it was about 22% of the student that cheated (the p variable).

        Summary Statistics
    parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

               p    0.2212    0.1192     0.0038    0.0063   307.7500    1.0006       28.8048
        obs_prop    0.3556    0.0435     0.0014    0.0028   377.7931    1.0154       35.3606
         p_prior    0.5006    0.2849     0.0090    0.0085   586.1921    1.0036       54.8664


   Cf cheating.jl and cheating2.jl for ports of the two PyMC3 models from the 
      "Probabilistic Programming Hacker" book.

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function cheating_model0(N,X)
    p ~ Uniform(0,1) # freq cheating
    tail = 0
    head = 1

    cheated      ~ filldist(Bernoulli(p),N) 
    truths       ~ filldist(Bernoulli(p),N)
    first_flips  ~ filldist(Bernoulli(0.5), N)
    second_flips ~ filldist(Bernoulli(0.5), N)

    # val = tzeros(N) # We must defined val to be used as LHS in @.

    # @. val = first_flips*truths + (1 - first_flips)*second_flips

    
    for i in 1:N 
        # """
        # In the interview process for each student, the student flips a coin, 
        # hidden from the interviewer. 
        # """
        if first_flips[i] == head 
            # The student agrees to answer honestly if the coin comes up heads. 
            # """
            if cheated[i] == true 
                truths[i] ~ Dirac(true)
            else 
                truths[i] ~ Dirac(false)
            end
            second_flips[i] ~ Dirac(true)
        else 
            # """
            # Otherwise, if the coin comes up tails, the student (secretly) flips the coin 
            # again, and answers "Yes, I did cheat" if the coin flip lands heads, 
            # and "No, I did not cheat", if the coin flip lands tails. 
            # """
            if second_flips[i] == head 
                truths[i] ~ Dirac(true)
            else 
                truths[i] ~ Dirac(false)
            end
        end
    end

    obs_prop ~ Dirac(sum(truths) / N) 
    X ~ Binomial(N, obs_prop )

    p_prior ~ Uniform(0,1)

end

N = 100
yes_response = 35
model = cheating_model0(N, yes_response)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5),  1_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns[[:p,:obs_prop,:p_prior]])
display(plot(chns[[:p,:obs_prop,:p_prior]]))

# Another way: 
println("mean(truths): $(mean(group(chns, :truths).value.data,dims=1) |> mean)")
println("mean(cheated): $(mean(group(chns, :cheated).value.data,dims=1) |> mean)")

